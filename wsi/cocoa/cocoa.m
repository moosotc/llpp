#include <Cocoa/Cocoa.h>
#include <OpenGL/gl.h>

#define CAML_NAME_SPACE

#include <sys/types.h>
#include <sys/socket.h>
#include <pthread.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/fail.h>

enum {
  EVENT_EXPOSE = 1,
  EVENT_RESHAPE = 3,
  EVENT_MOUSE = 4,
  EVENT_MOTION = 5,
  EVENT_PMOTION = 6,
  EVENT_KEYDOWN = 7,
  EVENT_ENTER = 8,
  EVENT_LEAVE = 9,
  EVENT_WINSTATE = 10,
  EVENT_QUIT = 11,
  EVENT_SCROLL = 12,
  EVENT_ZOOM = 13,
  EVENT_OPEN = 20
};

enum {
  BUTTON_LEFT = 1,
  BUTTON_RIGHT = 3,
  BUTTON_WHEEL_UP = 4,
  BUTTON_WHEEL_DOWN = 5
};

static int terminating = 0;
static pthread_mutex_t terminate_mutex = PTHREAD_MUTEX_INITIALIZER;
static int server_fd = -1;
static CGFloat backing_scale_factor = -1.0;

#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 10120

#define NS_CRITICAL_ALERT_STYLE NSAlertStyleCritical
#define NS_FULL_SCREEN_WINDOW_MASK NSWindowStyleMaskFullScreen
#define NS_DEVICE_INDEPENDENT_MODIFIER_FLAGS_MASK NSEventModifierFlagDeviceIndependentFlagsMask
#define NS_FUNCTION_KEY_MASK NSEventModifierFlagFunction
#define NS_ALTERNATE_KEY_MASK NSEventModifierFlagOption
#define NS_COMMAND_KEY_MASK NSEventModifierFlagCommand
#define NS_CLOSABLE_WINDOW_MASK NSWindowStyleMaskClosable
#define NS_MINIATURIZABLE_WINDOW_MASK NSWindowStyleMaskMiniaturizable
#define NS_TITLED_WINDOW_MASK NSWindowStyleMaskTitled
#define NS_RESIZABLE_WINDOW_MASK NSWindowStyleMaskResizable

#else

#define NS_CRITICAL_ALERT_STYLE NSCriticalAlertStyle
#define NS_FULL_SCREEN_WINDOW_MASK NSFullScreenWindowMask
#define NS_DEVICE_INDEPENDENT_MODIFIER_FLAGS_MASK NSDeviceIndependentModifierFlagsMask
#define NS_FUNCTION_KEY_MASK NSFunctionKeyMask
#define NS_ALTERNATE_KEY_MASK NSAlternateKeyMask
#define NS_COMMAND_KEY_MASK NSCommandKeyMask
#define NS_CLOSABLE_WINDOW_MASK NSClosableWindowMask
#define NS_MINIATURIZABLE_WINDOW_MASK NSMiniaturizableWindowMask
#define NS_TITLED_WINDOW_MASK NSTitledWindowMask
#define NS_RESIZABLE_WINDOW_MASK NSResizableWindowMask

#endif

void Abort (NSString *format, ...)
{
  va_list argList;
  va_start (argList, format);
  NSString *str = [[NSString alloc] initWithFormat:format arguments:argList];
  va_end (argList);
  NSLog (@"%@", str);
  NSAlert *alert = [[NSAlert alloc] init];
  [alert addButtonWithTitle:@"Quit"];
  [alert setMessageText:@"Internal Error"];
  [alert setInformativeText:str];
  [alert setAlertStyle:NS_CRITICAL_ALERT_STYLE];
  [alert runModal];
  [NSApp terminate:nil];
}

void *caml_main_thread (void *argv)
{
  @autoreleasepool {
    caml_main (argv);
    pthread_mutex_lock (&terminate_mutex);
    if (terminating == 0) {
      terminating = 1;
      [NSApp performSelectorOnMainThread:@selector(terminate:)
                              withObject:nil
                           waitUntilDone:NO];
    }
    pthread_mutex_unlock (&terminate_mutex);
  }
  pthread_exit (NULL);
}

NSCursor *GetCursor (int idx)
{
  static NSCursor *cursors[5];
  static BOOL initialised = NO;

  if (initialised == NO) {
    cursors[0] = [NSCursor arrowCursor];
    cursors[1] = [NSCursor pointingHandCursor];
    cursors[2] = [NSCursor arrowCursor];
    cursors[3] = [NSCursor closedHandCursor];
    cursors[4] = [NSCursor IBeamCursor];
    initialised = YES;
  }

  return cursors[idx];
}

@implementation NSWindow (CategoryNSWindow)

- (BOOL)isFullScreen
{
  return ([self styleMask] & NS_FULL_SCREEN_WINDOW_MASK) == NS_FULL_SCREEN_WINDOW_MASK;
}

@end

@implementation NSView (CategoryNSView)

- (NSPoint)locationFromEvent:(NSEvent *)event
{
  NSPoint point =
    [self convertPointToBacking:[self convertPoint:[event locationInWindow] fromView:nil]];
  NSRect bounds = [self convertRectToBacking:[self bounds]];
  point.y = bounds.size.height - point.y;
  return point;
}

- (NSRect)convertFrameToBacking
{
  return [self convertRectToBacking:[self frame]];
}

@end

@implementation NSEvent (CategoryNSEvent)

- (int)deviceIndependentModifierFlags
{
  return [self modifierFlags] & NS_DEVICE_INDEPENDENT_MODIFIER_FLAGS_MASK;
}

@end

@interface Connector : NSObject

- (instancetype)initWithFileDescriptor:(int)fd;

- (void)notifyReshapeWidth:(int)w height:(int)h;
- (void)notifyExpose;
- (void)keyDown:(uint32_t)key modifierFlags:(NSEventModifierFlags)mask;
- (void)notifyQuit;
- (void)mouseEntered:(NSPoint)loc;
- (void)mouseExited;
- (void)mouseMoved:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags;
- (void)mouseDown:(NSUInteger)buttons atPoint:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags;
- (void)mouseUp:(NSUInteger)buttons atPoint:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags;
@end

@implementation Connector
{
  NSMutableData *data;
  NSFileHandle *fileHandle;
}

- (instancetype)initWithFileDescriptor:(int)fd
{
  self = [super init];
  data = [NSMutableData dataWithLength:32];
  fileHandle = [[NSFileHandle alloc] initWithFileDescriptor:fd];
  return self;
}

- (void)setByte:(int8_t)b offset:(int)off
{
  [data replaceBytesInRange:NSMakeRange (off, 1) withBytes:&b];
}

- (void)setShort:(int16_t)s offset:(int)off
{
  [data replaceBytesInRange:NSMakeRange (off, 2) withBytes:&s];
}

- (void)setInt:(int32_t)n offset:(int)off
{
  [data replaceBytesInRange:NSMakeRange (off, 4) withBytes:&n];
}

- (void)writeData
{
  [fileHandle writeData:data];
}

- (void)notifyReshapeWidth:(int)w height:(int)h
{
  [self setByte:EVENT_RESHAPE offset:0];
  [self setShort:w offset:16];
  [self setShort:h offset:18];
  [self writeData];
}

- (void)notifyExpose
{
  [self setByte:EVENT_EXPOSE offset:0];
  [self writeData];
}

- (void)keyDown:(uint32_t)key modifierFlags:(NSEventModifierFlags)mask
{
  [self setByte:EVENT_KEYDOWN offset:0];
  [self setInt:key offset:16];
  [self setInt:mask offset:20];
  [self writeData];
}

- (void)notifyWinstate:(BOOL)fullScreen
{
  [self setByte:EVENT_WINSTATE offset:0];
  [self setInt:fullScreen offset:16];
  [self writeData];
}

- (void)notifyQuit
{
  [self setByte:EVENT_QUIT offset:0];
  [self writeData];
}

- (void)mouseEntered:(NSPoint)loc
{
  [self setByte:EVENT_ENTER offset:0];
  [self setShort:loc.x offset:16];
  [self setShort:loc.y offset:20];
  [self writeData];
}

- (void)mouseExited
{
  [self setByte:EVENT_LEAVE offset:0];
  [self writeData];
}

- (void)mouseDragged:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags
{
  [self setByte:EVENT_MOTION offset:0];
  [self setShort:aPoint.x offset:16];
  [self setShort:aPoint.y offset:20];
  [self setInt:flags offset:24];
  [self writeData];
}

- (void)mouseMoved:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags
{
  [self setByte:EVENT_PMOTION offset:0];
  [self setShort:aPoint.x offset:16];
  [self setShort:aPoint.y offset:20];
  [self setInt:flags offset:24];
  [self writeData];
}

- (void)mouseDown:(NSUInteger)buttons atPoint:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags
{
  [self setByte:EVENT_MOUSE offset:0];
  [self setShort:1 offset:10];
  [self setInt:buttons offset:12];
  [self setShort:aPoint.x offset:16];
  [self setShort:aPoint.y offset:20];
  [self setInt:flags offset:24];
  [self writeData];
}

- (void)mouseUp:(NSUInteger)buttons atPoint:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags
{
  [self setByte:EVENT_MOUSE offset:0];
  [self setShort:0 offset:10];
  [self setInt:buttons offset:12];
  [self setShort:aPoint.x offset:16];
  [self setShort:aPoint.y offset:20];
  [self setInt:flags offset:24];
  [self writeData];
}

- (void)scrollByDeltaX:(CGFloat)deltaX deltaY:(CGFloat)deltaY
{
  [self setByte:EVENT_SCROLL offset:0];
  [self setInt:(int32_t) deltaX offset:16];
  [self setInt:(int32_t) deltaY offset:20];
  [self writeData];
}

- (void)zoom:(CGFloat)z at:(NSPoint)p
{
  [self setByte:EVENT_ZOOM offset:0];
  [self setInt:(int32_t) (z * 1000) offset:16];
  [self setShort:p.x offset:20];
  [self setShort:p.y offset:22];
  [self writeData];
}

- (void)openFile:(NSString *)filename
{
  const char *utf8 = [filename UTF8String];
  unsigned len = [filename lengthOfBytesUsingEncoding:NSUTF8StringEncoding];
  [self setByte:EVENT_OPEN offset:0];
  unsigned off = 0;
  unsigned data_len = [data length] - 4;
  while (off < len) {
    unsigned chunk_len = MIN (data_len - 4, len - off);
    [self setShort:chunk_len offset:2];
    [data replaceBytesInRange:NSMakeRange (4, chunk_len) withBytes:(utf8 + off)];
    [self writeData];
    off += chunk_len;
  }
  [self setShort:0 offset:2];
  [self writeData];
}

@end

@interface MyDelegate : NSObject <NSApplicationDelegate, NSWindowDelegate>

- (int)getw;
- (int)geth;
- (void)swapb;
- (void)applicationWillFinishLaunching:(NSNotification *)not;
- (void)applicationDidFinishLaunching:(NSNotification *)not;
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication;
- (void)makeCurrentContext;

@end

@interface MyWindow : NSWindow

@end

@interface MyView : NSView
{
  Connector *connector;
  NSCursor *cursor;
}

- (instancetype)initWithFrame:(NSRect)frame connector:(Connector *)aConnector;
- (void)setCursor:(NSCursor *)aCursor;

@end

@implementation MyView

- (instancetype)initWithFrame:(NSRect)frame connector:(Connector *)aConnector
{
  self = [super initWithFrame:frame];

  if (self != NULL) {
    connector = aConnector;
    cursor = [NSCursor arrowCursor];
    self.allowedTouchTypes = NSTouchTypeMaskDirect | NSTouchTypeMaskIndirect;
  }

  return self;
}

- (void)setCursor:(NSCursor *)aCursor
{
  cursor = aCursor;
}

-(void)resetCursorRects
{
  [self addCursorRect:[self bounds] cursor:cursor];
}

- (void)drawRect:(NSRect)bounds
{
  // NSLog(@"drawRect: %@", [NSValue valueWithRect:bounds]);
  [connector notifyExpose];
}

- (void)viewWillMoveToWindow:(NSWindow *)newWindow {
  NSTrackingArea* trackingArea = [[NSTrackingArea alloc]
                                   initWithRect:[self bounds]
                                 options:(NSTrackingMouseEnteredAndExited | NSTrackingActiveInActiveApp | NSTrackingInVisibleRect)
                                          owner:self
                                       userInfo:nil];
  [self addTrackingArea:trackingArea];
}

- (void)keyDown:(NSEvent *)event
{
  // int key = [event keyCode];
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  NSString *chars = [event charactersIgnoringModifiers];
  const uint32_t *c = (uint32_t *) [chars cStringUsingEncoding:NSUTF32LittleEndianStringEncoding];
  while (*c) {
    if (*c == 0x7f && !(mask & NS_FUNCTION_KEY_MASK)) {
      [connector keyDown:0x8 modifierFlags:mask];
    } else {
      [connector keyDown:*c modifierFlags:mask];
    }
    c++;
  }
}

- (void)flagsChanged:(NSEvent *)event
{
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  NSLog (@"flagsChanged: 0x%lx", mask);
  if (mask != 0) {
    [connector keyDown:0 modifierFlags:mask];
  }
}

- (void)mouseDown:(NSEvent *)event
{
  [connector mouseDown:BUTTON_LEFT
               atPoint:[self locationFromEvent:event]
         modifierFlags:[event deviceIndependentModifierFlags]];
}

- (void)mouseUp:(NSEvent *)event
{
  [connector mouseUp:BUTTON_LEFT
             atPoint:[self locationFromEvent:event]
       modifierFlags:[event deviceIndependentModifierFlags]];
}

- (void)rightMouseDown:(NSEvent *)event
{
  [connector mouseDown:BUTTON_RIGHT
               atPoint:[self locationFromEvent:event]
         modifierFlags:[event deviceIndependentModifierFlags]];
}

- (void)rightMouseUp:(NSEvent *)event
{
  [connector mouseUp:BUTTON_RIGHT
             atPoint:[self locationFromEvent:event]
       modifierFlags:[event deviceIndependentModifierFlags]];
}

- (void)rightMouseDragged:(NSEvent *)event
{
  [connector mouseDragged:[self locationFromEvent:event]
            modifierFlags:[event deviceIndependentModifierFlags]];
}

- (void)mouseDragged:(NSEvent *)event
{
  [connector mouseDragged:[self locationFromEvent:event]
            modifierFlags:[event deviceIndependentModifierFlags]];
}

- (void)mouseMoved:(NSEvent *)event
{
  [connector mouseMoved:[self locationFromEvent:event]
          modifierFlags:[event deviceIndependentModifierFlags]];
}

- (void)mouseEntered:(NSEvent *)event
{
  [connector mouseEntered:[self locationFromEvent:event]];
}

- (void)mouseExited:(NSEvent *)event
{
  [connector mouseExited];
}

- (void)scrollWheel:(NSEvent *)event
{
  CGFloat deltaX = [event scrollingDeltaX];
  CGFloat deltaY = -[event scrollingDeltaY];

  if ([event hasPreciseScrollingDeltas]) {
    [connector scrollByDeltaX:(backing_scale_factor * deltaX)
                       deltaY:(backing_scale_factor * deltaY)];
  } else {
    NSPoint loc = [self locationFromEvent:event];
    NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
    if (deltaY > 0.0) {
      [connector mouseDown:BUTTON_WHEEL_DOWN atPoint:loc modifierFlags:mask];
      [connector mouseUp:BUTTON_WHEEL_DOWN atPoint:loc modifierFlags:mask];
    } else if (deltaY < 0.0) {
      [connector mouseDown:BUTTON_WHEEL_UP atPoint:loc modifierFlags:mask];
      [connector mouseUp:BUTTON_WHEEL_UP atPoint:loc modifierFlags:mask];
    }
  }
}

- (void)magnifyWithEvent:(NSEvent *)event
{
  [connector zoom:[event magnification] at:[self locationFromEvent:event]];
}

@end

@implementation MyWindow

- (BOOL)canBecomeKeyWindow
{
  return YES;
}

@end

@implementation MyDelegate
{
  char **argv;
  MyWindow *window;
  NSOpenGLContext *glContext;
  pthread_t thread;
  Connector *connector;
}

- (instancetype)initWithArgv:(char **)theArgv fileDescriptor:(int)fd
{
  self = [super init];
  if (self != NULL) {
    argv = theArgv;
    connector = [[Connector alloc] initWithFileDescriptor:fd];
  }
  return self;
}

- (void)setTitle:(NSString *)title
{
  [window setTitle:title];
}

- (void)mapwin
{
  [window makeKeyAndOrderFront:self];
}

- (int)getw
{
  return [[window contentView] convertFrameToBacking].size.width;
}

- (int)geth
{
  return [[window contentView] convertFrameToBacking].size.height;
}

- (void)applicationWillFinishLaunching:(NSNotification *)not
{
  NSLog(@"applicationWillFinishLaunching");
  id menubar = [NSMenu new];
  id appMenuItem = [NSMenuItem new];
  id fileMenuItem = [NSMenuItem new];
  id windowMenuItem = [NSMenuItem new];
  id helpMenuItem = [NSMenuItem new];
  [menubar addItem:appMenuItem];
  [menubar addItem:fileMenuItem];
  [menubar addItem:windowMenuItem];
  [menubar addItem:helpMenuItem];
  [NSApp setMainMenu:menubar];
  id appMenu = [NSMenu new];
  id appName = [[NSProcessInfo processInfo] processName];
  id aboutMenuItem = [[NSMenuItem alloc] initWithTitle:[@"About " stringByAppendingString:appName]
                                                action:@selector(orderFrontStandardAboutPanel:)
                                         keyEquivalent:@""];
  id hideMenuItem = [[NSMenuItem alloc] initWithTitle:[@"Hide " stringByAppendingString:appName]
                                               action:@selector(hide:)
                                        keyEquivalent:@"h"];
  id hideOthersMenuItem = [[NSMenuItem alloc] initWithTitle:@"Hide Others"
                                                     action:@selector(hideOtherApplications:)
                                              keyEquivalent:@"h"];
  [hideOthersMenuItem setKeyEquivalentModifierMask:(NS_ALTERNATE_KEY_MASK | NS_COMMAND_KEY_MASK)];
  id showAllMenuItem = [[NSMenuItem alloc] initWithTitle:@"Show All"
                                                  action:@selector(unhideAllApplications:)
                                           keyEquivalent:@""];
  id quitMenuItem = [[NSMenuItem alloc] initWithTitle:[@"Quit " stringByAppendingString:appName]
                                               action:@selector(terminate:)
                                        keyEquivalent:@"q"];
  [appMenu addItem:aboutMenuItem];
  [appMenu addItem:[NSMenuItem separatorItem]];
  [appMenu addItem:hideMenuItem];
  [appMenu addItem:hideOthersMenuItem];
  [appMenu addItem:showAllMenuItem];
  [appMenu addItem:[NSMenuItem separatorItem]];
  [appMenu addItem:quitMenuItem];
  [appMenuItem setSubmenu:appMenu];

  id fileMenu = [[NSMenu alloc] initWithTitle:@"File"];
  id openMenuItem = [[NSMenuItem alloc] initWithTitle:@"Open..."
                                               action:@selector(openDocument:)
                                        keyEquivalent:@"o"];
  id closeMenuItem = [[NSMenuItem alloc] initWithTitle:@"Close"
                                                action:@selector(performClose:)
                                         keyEquivalent:@"w"];
  [fileMenu addItem:openMenuItem];
  [fileMenu addItem:[NSMenuItem separatorItem]];
  [fileMenu addItem:closeMenuItem];
  [fileMenuItem setSubmenu:fileMenu];

  id windowMenu = [[NSMenu alloc] initWithTitle:@"Window"];
  id miniaturizeMenuItem = [[NSMenuItem alloc] initWithTitle:@"Minimize"
                                                   action:@selector(performMiniaturize:)
                                            keyEquivalent:@"m"];
  id zoomMenuItem = [[NSMenuItem alloc] initWithTitle:@"Zoom"
                                               action:@selector(performZoom:)
                                        keyEquivalent:@""];

  [windowMenu addItem:miniaturizeMenuItem];
  [windowMenu addItem:zoomMenuItem];
  [windowMenuItem setSubmenu:windowMenu];

  id helpMenu = [[NSMenu alloc] initWithTitle:@"Help"];
  id reportIssueMenuItem = [[NSMenuItem alloc] initWithTitle:@"Report an issue..."
                                                      action:@selector(reportIssue:)
                                               keyEquivalent:@""];
  [helpMenu addItem:reportIssueMenuItem];
  [helpMenuItem setSubmenu:helpMenu];

  window = [[MyWindow alloc] initWithContentRect:NSMakeRect(0, 0, 400, 400)
                                       styleMask:(NS_CLOSABLE_WINDOW_MASK | NS_MINIATURIZABLE_WINDOW_MASK | NS_TITLED_WINDOW_MASK | NS_RESIZABLE_WINDOW_MASK)
                                         backing:NSBackingStoreBuffered
                                           defer:NO];

  [window center];
  [window setAcceptsMouseMovedEvents:YES];
  [window setDelegate:self];


  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(didEnterFullScreen)
                                               name:NSWindowDidEnterFullScreenNotification
                                             object:window];
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(didExitFullScreen)
                                               name:NSWindowDidExitFullScreenNotification
                                             object:window];

  MyView *myView = [[MyView alloc] initWithFrame:[[window contentView] bounds]
                                       connector:connector];

  [window setContentView:myView];
  [window makeFirstResponder:myView];

  [myView setWantsBestResolutionOpenGLSurface:YES];

  NSOpenGLPixelFormatAttribute attrs[] =
    {
      NSOpenGLPFAAccelerated,
      NSOpenGLPFADoubleBuffer,
      NSOpenGLPFAColorSize, 24,
      NSOpenGLPFAAlphaSize, 8,
      NSOpenGLPFADepthSize, 24,
      0
    };
  NSOpenGLPixelFormat *pixFormat = [[NSOpenGLPixelFormat alloc] initWithAttributes:attrs];
  glContext = [[NSOpenGLContext alloc] initWithFormat:pixFormat shareContext:nil];
  GLint swapInt = 1;
  [glContext setValues:&swapInt forParameter:NSOpenGLCPSwapInterval];
  [glContext setView:myView];

  backing_scale_factor = [window backingScaleFactor];
}

- (void)reshape:(NSValue *)val
{
  // NSLog (@"reshape: %@ isFullScreen: %d", val, [window isFullScreen]);
  if ([window isFullScreen]) {
    [window toggleFullScreen:self];
  }
  [window setFrame:[window frameRectForContentRect:[val rectValue]]
           display:YES];
}

- (void)makeCurrentContext
{
  [glContext makeCurrentContext];
  NSLog (@"OpenGL Version: %s", glGetString(GL_VERSION));
}

- (void)swapb
{
  [glContext flushBuffer];
}

- (void)didEnterFullScreen
{
  // NSLog (@"didEnterFullScreen: %d", [window isFullScreen]);
  [connector notifyWinstate:YES];
}

- (void)didExitFullScreen
{
  // NSLog (@"didExitFullScreen: %d", [window isFullScreen]);
  [connector notifyWinstate:NO];
}

- (void)fullscreen
{
  // NSLog (@"fullscreen: %d", [window isFullScreen]);
  if ([window isFullScreen] == NO) {
    [window toggleFullScreen:self];
  }
}

- (void)setCursor:(NSCursor *)aCursor
{
  [[window contentView] setCursor: aCursor];
  [window invalidateCursorRectsForView:[window contentView]];
}

- (void)windowDidResize:(NSNotification *)notification
{
  [glContext update];
  NSRect frame = [[window contentView] convertFrameToBacking];
  [connector notifyReshapeWidth:frame.size.width height:frame.size.height];
}

- (void)windowDidMove:(NSNotification *)notification
{
  [glContext update];
}

- (void)applicationWillTerminate:(NSDictionary *)userInfo
{
  pthread_mutex_lock (&terminate_mutex);
  if (terminating == 0) {
    terminating = 1;
    [connector notifyQuit];
  }
  pthread_mutex_unlock (&terminate_mutex);
  pthread_join (thread, NULL);
}

- (void)windowDidChangeOcclusionState:(NSNotification *)notification
{
}

- (void)applicationDidFinishLaunching:(NSNotification *)not
{
  NSLog(@"applicationDidFinishLaunching");
  int ret = pthread_create (&thread, NULL, caml_main_thread, argv);
  if (ret != 0) {
    Abort (@"pthread_create: %s.", strerror (ret));
  }
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication
{
  return YES;
}

- (BOOL)application:(NSApplication *)theApplication openFile:(NSString *)filename
{
  NSLog (@"openFile: %@", filename);
  [connector openFile:filename];
  return YES;
}

- (void)openDocument:(id)sender
{
  NSOpenPanel *openPanel = [NSOpenPanel openPanel];
  [openPanel beginSheetModalForWindow:window
                    completionHandler:^(NSInteger result){
      if (result == NSModalResponseOK) {
         NSString *filename = [[[openPanel URLs] objectAtIndex:0] path];
         if (filename != nil) {
           [self application:NSApp openFile:filename];
         }
      }
    }];
}

- (void)reportIssue:(id)sender
{
  [[NSWorkspace sharedWorkspace]
    openURL:[NSURL URLWithString:@"https://github.com/moosotc/llpp/issues"]];
}

@end

CAMLprim value ml_mapwin (value unit)
{
  CAMLparam1 (unit);
  [(MyDelegate *)[NSApp delegate] performSelectorOnMainThread:@selector(mapwin)
                                                   withObject:nil
                                                waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_swapb (value unit)
{
  CAMLparam1 (unit);
  [(MyDelegate *)[NSApp delegate] swapb];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_getw (value unit)
{
  return Val_int([(MyDelegate *)[NSApp delegate] getw]);
}

CAMLprim value ml_geth (value unit)
{
  return Val_int([(MyDelegate *)[NSApp delegate] geth]);
}

CAMLprim value ml_makecurrentcontext (value unit)
{
  CAMLparam1 (unit);
  [(MyDelegate *)[NSApp delegate] makeCurrentContext];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_settitle (value title)
{
  CAMLparam1 (title);
  NSString *str = [NSString stringWithUTF8String:String_val(title)];
  [(MyDelegate *)[NSApp delegate] performSelectorOnMainThread:@selector(setTitle:)
                                                   withObject:str
                                                waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_reshape (value w, value h)
{
  CAMLparam2 (w, h);
  NSRect r = NSMakeRect (0, 0, Int_val (w), Int_val (h));
  [(MyDelegate *)[NSApp delegate] performSelectorOnMainThread:@selector(reshape:)
                                                   withObject:[NSValue valueWithRect:r]
                                                waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_fullscreen (value unit)
{
  CAMLparam1 (unit);
  [(MyDelegate *)[NSApp delegate] performSelectorOnMainThread:@selector(fullscreen)
                                                   withObject:nil
                                                waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_setcursor (value curs)
{
  CAMLparam1 (curs);
  // NSLog (@"ml_setcursor: %d", Int_val (curs));
  NSCursor *cursor = GetCursor (Int_val (curs));
  [(MyDelegate *)[NSApp delegate] performSelectorOnMainThread:@selector(setCursor:)
                                                   withObject:cursor
                                                waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_get_server_fd (value unit)
{
  CAMLparam1 (unit);
  CAMLreturn (Val_int (server_fd));
}

CAMLprim value ml_get_backing_scale_factor (value unit)
{
  CAMLparam1 (unit);
  CAMLreturn (Val_int ((int) backing_scale_factor));
}

CAMLprim value ml_nslog (value str)
{
  CAMLparam1 (str);
  NSLog (@"%s", String_val (str));
  CAMLreturn (Val_unit);
}

// HACK to eliminate arg injected by OS X -psn_...
int adjust_argv (int argc, char **argv)
{
  if (argc > 1 && strncmp (argv[1], "-psn", 4) == 0) {
    for (unsigned i = 1; i < argc - 1; i ++) {
      argv[i] = argv[i+1];
    }
    argv[-- argc] = 0;
  }
  for (int i = 0; i < argc; i ++) {
    NSLog (@"arg %d: %s", i, argv[i]);
  }
  return argc;
}

void (*wsigladdr (const char *name)) (void)
{
  static CFBundleRef framework = NULL;
  if (framework == NULL)
    framework = CFBundleGetBundleWithIdentifier (CFSTR ("com.apple.opengl"));

  char *bytes;
  CFStringRef str;
  bytes = CFAllocatorAllocate (CFAllocatorGetDefault(), 6, 0);
  strcpy (bytes, name);
  str = CFStringCreateWithCStringNoCopy (NULL, bytes,
                                         kCFStringEncodingMacRoman, NULL);
  void (*ret) (void) = CFBundleGetFunctionPointerForName (framework, str);
  CFRelease (str);
  return ret;
}

int main(int argc, char **argv)
{
  @autoreleasepool {
    int sv[2];
    int ret = socketpair (AF_UNIX, SOCK_STREAM, 0, sv);
    if (ret != 0) {
      Abort (@"socketpair: %s", strerror (errno));
    }
    // NSLog (@"socketpair sv0 %d sv1 %d", sv[0], sv[1]);
    server_fd = sv[0];
    argc = adjust_argv (argc, argv);
    [NSApplication sharedApplication];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
    id delegate = [[MyDelegate alloc] initWithArgv:argv fileDescriptor:sv[1]];
    [NSApp setDelegate:delegate];
    [NSApp activateIgnoringOtherApps:YES];
    [NSApp run];
  }
  return EXIT_SUCCESS;
}
