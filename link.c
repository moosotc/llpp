/* lots of code c&p-ed directly from mupdf */
#define FIXME 0

#ifdef __clang__
#pragma GCC diagnostic error "-Weverything"
#pragma GCC diagnostic ignored "-Wpadded"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wdocumentation-unknown-command"
#pragma GCC diagnostic ignored "-Wdocumentation"
#pragma GCC diagnostic ignored "-Wdouble-promotion"
#endif

extern char **environ;

#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>

#include <math.h>
#include <wchar.h>
#include <locale.h>
#include <langinfo.h>

#include <unistd.h>
#include <pthread.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/utsname.h>

#include <spawn.h>

#include <regex.h>
#include <stdarg.h>
#include <limits.h>
#include <inttypes.h>

#ifdef CIDER
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif

#pragma GCC diagnostic push
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wreserved-id-macro"
#endif
#pragma GCC diagnostic ignored "-Wpedantic"
#define CAML_NAME_SPACE
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
#include <mupdf/fitz.h>
#include <mupdf/pdf.h>
#pragma GCC diagnostic pop

#include <ft2build.h>
#include FT_FREETYPE_H
#pragma GCC diagnostic pop

#include "cutils.h"

#ifdef USE_NPOT
#define TEXT_TYPE GL_TEXTURE_2D
#else
#define TEXT_TYPE GL_TEXTURE_RECTANGLE_ARB
#endif

#if 0
#define lprintf printf
#else
#define lprintf(...)
#endif

#define ARSERT(cond) for (;;) {                         \
    if (!(cond)) {                                      \
        errx (1, "%s:%d " #cond, __FILE__, __LINE__);   \
    }                                                   \
    break;                                              \
}

struct slice {
    int h;
    int texindex;
};

struct tile {
    int w, h;
    int slicecount;
    int sliceheight;
    struct bo *pbo;
    fz_pixmap *pixmap;
    struct slice slices[1];
};

struct pagedim {
    int pageno;
    int rotate;
    int left;
    int tctmready;
    fz_irect bounds;
    fz_rect pagebox;
    fz_rect mediabox;
    fz_matrix ctm, zoomctm, tctm;
};

struct slink {
    enum { SLINK, SANNOT } tag;
    fz_irect bbox;
    union {
        fz_link *link;
        pdf_annot *annot;
    } u;
};

struct annot {
    fz_irect bbox;
    pdf_annot *annot;
};

struct page {
    int tgen;
    int sgen;
    int agen;
    int pageno;
    int pdimno;
    fz_stext_page *text;
    fz_page *fzpage;
    fz_display_list *dlist;
    fz_link *links;
    int slinkcount;
    struct slink *slinks;
    int annotcount;
    struct annot *annots;
    fz_stext_char *fmark, *lmark;
};

enum { FitWidth, FitProportional, FitPage };

static struct {
    int sliceheight;
    struct pagedim *pagedims;
    int pagecount;
    int pagedimcount;
    fz_document *doc;
    fz_context *ctx;
    int w, h;

    int texindex;
    int texcount;
    GLuint *texids;

    GLenum texiform;
    GLenum texform;
    GLenum texty;

    fz_colorspace *colorspace;

    struct {
        int w, h;
        struct slice *slice;
    } *texowners;

    int rotate;
    int fitmodel;
    int trimmargins;
    int needoutline;
    int gen;
    int aalevel;

    int trimanew;
    fz_irect trimfuzz;
    fz_pixmap *pig;

    pthread_t thread;
    int csock;
    FT_Face face;

    char *trimcachepath;
    int dirty;

    GLuint stid;

    int bo_usable;
    GLuint boid;

    void (*glBindBufferARB) (GLenum, GLuint);
    GLboolean (*glUnmapBufferARB) (GLenum);
    void *(*glMapBufferARB) (GLenum, GLenum);
    void (*glBufferDataARB) (GLenum, GLsizei, void *, GLenum);
    void (*glGenBuffersARB) (GLsizei, GLuint *);
    void (*glDeleteBuffersARB) (GLsizei, GLuint *);

    GLfloat texcoords[8];
    GLfloat vertices[16];

    int utf8cs;
} state;

struct bo {
    GLuint id;
    void *ptr;
    size_t size;
};

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

static void lock (const char *cap)
{
    int ret = pthread_mutex_lock (&mutex);
    if (ret) {
        errx (1, "%s: pthread_mutex_lock: %s", cap, strerror (ret));
    }
}

static void unlock (const char *cap)
{
    int ret = pthread_mutex_unlock (&mutex);
    if (ret) {
        errx (1, "%s: pthread_mutex_unlock: %s", cap, strerror (ret));
    }
}

static int trylock (const char *cap)
{
    int ret = pthread_mutex_trylock (&mutex);
    if (ret && ret != EBUSY) {
        errx (1, "%s: pthread_mutex_trylock: %s", cap, strerror (ret));
    }
    return ret == EBUSY;
}

static int hasdata (void)
{
    int ret, avail;
    ret = ioctl (state.csock, FIONREAD, &avail);
    if (ret) err (1, "hasdata: FIONREAD error ret=%d", ret);
    return avail > 0;
}

value ml_hasdata (value fd_v);
value ml_hasdata (value fd_v)
{
    CAMLparam1 (fd_v);
    int ret, avail;

    ret = ioctl (Int_val (fd_v), FIONREAD, &avail);
    if (ret) uerror ("ioctl (FIONREAD)", Nothing);
    CAMLreturn (Val_bool (avail > 0));
}

static void readdata (int fd, void *p, int size)
{
    ssize_t n;

again:
    n = read (fd, p, size);
    if (n - size) {
        if (n < 0 && errno == EINTR) goto again;
        if (!n) errx (1, "EOF while reading");
        errx (1, "read (fd %d, req %d, ret %zd)", fd, size, n);
    }
}

static void writedata (int fd, char *p, int size)
{
    ssize_t n;
    uint32_t size4 = size;
    struct iovec iov[2] = {
        { .iov_base = &size4, .iov_len = 4 },
        { .iov_base = p, .iov_len = size }
    };

again:
    n = writev (fd, iov, 2);
    if (n < 0 && errno == EINTR) goto again;
    if (n - size - 4) {
        if (!n) errx (1, "EOF while writing data");
        err (1, "writev (fd %d, req %d, ret %zd)", fd, size + 4, n);
    }
}

static int readlen (int fd)
{
    uint32_t u;
    readdata (fd, &u, 4);
    return u;
}

void ml_wcmd (value fd_v, value bytes_v, value len_v);
void ml_wcmd (value fd_v, value bytes_v, value len_v)
{
    CAMLparam3 (fd_v, bytes_v, len_v);
    writedata (Int_val (fd_v), &Byte (bytes_v, 0), Int_val (len_v));
    CAMLreturn0;
}

value ml_rcmd (value fd_v);
value ml_rcmd (value fd_v)
{
    CAMLparam1 (fd_v);
    CAMLlocal1 (strdata_v);
    int fd = Int_val (fd_v);
    int len = readlen (fd);
    strdata_v = caml_alloc_string (len);
    readdata (fd, String_val (strdata_v), len);
    CAMLreturn (strdata_v);
}

static void GCC_FMT_ATTR (1, 2) printd (const char *fmt, ...)
{
    char fbuf[64];
    int size = sizeof (fbuf), len;
    va_list ap;
    char *buf = fbuf;

    for (;;) {
        va_start (ap, fmt);
        len = vsnprintf (buf, size, fmt, ap);
        va_end (ap);

        if (len > -1) {
            if (len < size - 4) {
                writedata (state.csock, buf, len);
                break;
            }
            else size = len + 5;
        }
        else {
            err (1, "vsnprintf for `%s' failed", fmt);
        }
        buf = realloc (buf == fbuf ? NULL : buf, size);
        if (!buf) err (1, "realloc for temp buf (%d bytes) failed", size);
    }
    if (buf != fbuf) free (buf);
}

static void closedoc (void)
{
    if (state.doc) {
        fz_drop_document (state.ctx, state.doc);
        state.doc = NULL;
    }
}

static int openxref (char *filename, char *password, int layouth)
{
    for (int i = 0; i < state.texcount; ++i) {
        state.texowners[i].w = -1;
        state.texowners[i].slice = NULL;
    }

    closedoc ();

    state.dirty = 0;
    if (state.pagedims) {
        free (state.pagedims);
        state.pagedims = NULL;
    }
    state.pagedimcount = 0;

    fz_set_aa_level (state.ctx, state.aalevel);
    state.doc = fz_open_document (state.ctx, filename);
    if (fz_needs_password (state.ctx, state.doc)) {
        if (password && !*password) {
            printd ("pass");
            return 0;
        }
        else {
            int ok = fz_authenticate_password (state.ctx, state.doc, password);
            if (!ok) {
                printd ("pass fail");
                return 0;
            }
        }
    }
    if (layouth >= 0)
        fz_layout_document (state.ctx, state.doc, 460, layouth, 12);
    state.pagecount = fz_count_pages (state.ctx, state.doc);
    return 1;
}

static void docinfo (void)
{
    struct { char *tag; char *name; } metatbl[] = {
        { FZ_META_INFO_TITLE, "Title" },
        { FZ_META_INFO_AUTHOR, "Author" },
        { FZ_META_FORMAT, "Format" },
        { FZ_META_ENCRYPTION, "Encryption" },
        { "info:Creator", "Creator" },
        { "info:Producer", "Producer" },
        { "info:CreationDate", "Creation date" },
    };
    int len = 0;
    char *buf = NULL;

    for (size_t i = 0; i < sizeof (metatbl) / sizeof (metatbl[1]); ++i) {
        int need;
    again:
        need = fz_lookup_metadata (state.ctx, state.doc,
                                   metatbl[i].tag, buf, len);
        if (need > 0) {
            if (need <= len) {
                printd ("info %s\t%s", metatbl[i].name, buf);
            }
            else {
                buf = realloc (buf, need + 1);
                if (!buf) err (1, "docinfo realloc %d", need + 1);
                len = need + 1;
                goto again;
            }
        }
    }
    free (buf);

    printd ("infoend");
}

static void unlinktile (struct tile *tile)
{
    for (int i = 0; i < tile->slicecount; ++i) {
        struct slice *s = &tile->slices[i];

        if (s->texindex != -1) {
            if (state.texowners[s->texindex].slice == s) {
                state.texowners[s->texindex].slice = NULL;
            }
        }
    }
}

static void freepage (struct page *page)
{
    if (!page) return;
    if (page->text) {
        fz_drop_stext_page (state.ctx, page->text);
    }
    if (page->slinks) {
        free (page->slinks);
    }
    fz_drop_display_list (state.ctx, page->dlist);
    fz_drop_page (state.ctx, page->fzpage);
    free (page);
}

static void freetile (struct tile *tile)
{
    unlinktile (tile);
    if (!tile->pbo) {
#if 0
        fz_drop_pixmap (state.ctx, tile->pixmap);
#else  /* piggyback */
        if (state.pig) {
            fz_drop_pixmap (state.ctx, state.pig);
        }
        state.pig = tile->pixmap;
#endif
    }
    else {
        free (tile->pbo);
        fz_drop_pixmap (state.ctx, tile->pixmap);
    }
    free (tile);
}

static void trimctm (pdf_page *page, int pindex)
{
    struct pagedim *pdim = &state.pagedims[pindex];

    if (!page) return;
    if (!pdim->tctmready) {
        fz_rect realbox, mediabox;
        fz_matrix page_ctm, ctm;

        ctm = fz_concat (fz_rotate (-pdim->rotate), fz_scale (1, -1));
        realbox = fz_transform_rect (pdim->mediabox, ctm);
        pdf_page_transform (state.ctx, page, &mediabox, &page_ctm);
        pdim->tctm = fz_concat (
            fz_invert_matrix (page_ctm),
            fz_concat (ctm, fz_translate (-realbox.x0, -realbox.y0)));
        pdim->tctmready = 1;
    }
}

static fz_matrix pagectm1 (fz_page *fzpage, struct pagedim *pdim)
{
    fz_matrix ctm;
    ptrdiff_t pdimno = pdim - state.pagedims;

    ARSERT (pdim - state.pagedims < INT_MAX);
    if (pdf_specifics (state.ctx, state.doc)) {
        trimctm (pdf_page_from_fz_page (state.ctx, fzpage), (int) pdimno);
        ctm = fz_concat (pdim->tctm, pdim->ctm);
    }
    else {
        ctm = fz_concat (fz_translate (-pdim->mediabox.x0, -pdim->mediabox.y0),
                         pdim->ctm);
    }
    return ctm;
}

static fz_matrix pagectm (struct page *page)
{
    return pagectm1 (page->fzpage, &state.pagedims[page->pdimno]);
}

static void *loadpage (int pageno, int pindex)
{
    fz_device *dev;
    struct page *page;

    page = calloc (sizeof (struct page), 1);
    if (!page) {
        err (1, "calloc page %d", pageno);
    }

    page->dlist = fz_new_display_list (state.ctx, fz_infinite_rect);
    dev = fz_new_list_device (state.ctx, page->dlist);
    fz_try (state.ctx) {
        page->fzpage = fz_load_page (state.ctx, state.doc, pageno);
        fz_run_page (state.ctx, page->fzpage, dev, fz_identity, NULL);
    }
    fz_catch (state.ctx) {
        page->fzpage = NULL;
    }
    fz_close_device (state.ctx, dev);
    fz_drop_device (state.ctx, dev);

    page->pdimno = pindex;
    page->pageno = pageno;
    page->sgen = state.gen;
    page->agen = state.gen;
    page->tgen = state.gen;
    return page;
}

static struct tile *alloctile (int h)
{
    int slicecount;
    size_t tilesize;
    struct tile *tile;

    slicecount = (h + state.sliceheight - 1) / state.sliceheight;
    tilesize = sizeof (*tile) + ((slicecount - 1) * sizeof (struct slice));
    tile = calloc (tilesize, 1);
    if (!tile) {
        err (1, "cannot allocate tile (%zu bytes)", tilesize);
    }
    for (int i = 0; i < slicecount; ++i) {
        int sh = fz_mini (h, state.sliceheight);
        tile->slices[i].h = sh;
        tile->slices[i].texindex = -1;
        h -= sh;
    }
    tile->slicecount = slicecount;
    tile->sliceheight = state.sliceheight;
    return tile;
}

static struct tile *rendertile (struct page *page, int x, int y, int w, int h,
                                struct bo *pbo)
{
    fz_irect bbox;
    fz_matrix ctm;
    fz_device *dev;
    struct tile *tile;
    struct pagedim *pdim;

    tile = alloctile (h);
    pdim = &state.pagedims[page->pdimno];

    bbox = pdim->bounds;
    bbox.x0 += x;
    bbox.y0 += y;
    bbox.x1 = bbox.x0 + w;
    bbox.y1 = bbox.y0 + h;

    if (state.pig) {
        if (state.pig->w == w
            && state.pig->h == h
            && state.pig->colorspace == state.colorspace) {
            tile->pixmap = state.pig;
            tile->pixmap->x = bbox.x0;
            tile->pixmap->y = bbox.y0;
        }
        else {
            fz_drop_pixmap (state.ctx, state.pig);
        }
        state.pig = NULL;
    }
    if (!tile->pixmap) {
        if (pbo) {
            tile->pixmap =
                fz_new_pixmap_with_bbox_and_data (state.ctx, state.colorspace,
                                                  bbox, NULL, 1, pbo->ptr);
            tile->pbo = pbo;
        }
        else {
            tile->pixmap =
                fz_new_pixmap_with_bbox (state.ctx, state.colorspace, bbox,
                                         NULL, 1);
        }
    }

    tile->w = w;
    tile->h = h;
    fz_clear_pixmap_with_value (state.ctx, tile->pixmap, 0xff);

    dev = fz_new_draw_device (state.ctx, fz_identity, tile->pixmap);
    ctm = pagectm (page);
    fz_run_display_list (state.ctx, page->dlist, dev, ctm,
                         fz_rect_from_irect (bbox), NULL);
    fz_close_device (state.ctx, dev);
    fz_drop_device (state.ctx, dev);

    return tile;
}

static void initpdims (void)
{
    FILE *trimf = NULL;
    fz_rect rootmediabox = fz_empty_rect;
    int pageno, trim, show;
    int trimw = 0, cxcount;
    fz_context *ctx = state.ctx;
    pdf_document *pdf = pdf_specifics (ctx, state.doc);

    fz_var (trimw);
    fz_var (trimf);
    fz_var (cxcount);

    if (state.trimmargins && state.trimcachepath) {
        trimf = fopen (state.trimcachepath, "rb");
        if (!trimf) {
            trimf = fopen (state.trimcachepath, "wb");
            trimw = 1;
        }
    }

    cxcount = state.pagecount;
    if (pdf) {
        pdf_obj *obj;
        obj = pdf_dict_getp (ctx, pdf_trailer (ctx, pdf),
                             "Root/Pages/MediaBox");
        rootmediabox = pdf_to_rect (ctx, obj);
        pdf_load_page_tree (ctx, pdf);
    }

    for (pageno = 0; pageno < cxcount; ++pageno) {
        int rotate = 0;
        struct pagedim *p;
        fz_rect mediabox = fz_empty_rect;

        fz_var (rotate);
        if (pdf) {
            pdf_obj *pageobj = NULL;

            fz_var (pageobj);
            if (pdf->rev_page_map) {
                for (int i = 0; i < pdf->rev_page_count; ++i) {
                    if (pdf->rev_page_map[i].page == pageno) {
                        pageobj = pdf_get_xref_entry (
                            ctx, pdf, pdf->rev_page_map[i].object
                            )->obj;
                        break;
                    }
                }
            }
            if (!pageobj) {
                pageobj = pdf_lookup_page_obj (ctx, pdf, pageno);
            }

            rotate = pdf_to_int (ctx, pdf_dict_gets (ctx, pageobj, "Rotate"));

            if (state.trimmargins) {
                pdf_obj *obj;
                pdf_page *page;

                fz_try (ctx) {
                    page = pdf_load_page (ctx, pdf, pageno);
                    obj = pdf_dict_gets (ctx, pageobj, "llpp.TrimBox");
                    trim = state.trimanew || !obj;
                    if (trim) {
                        fz_rect rect;
                        fz_device *dev;
                        fz_matrix ctm, page_ctm;

                        dev = fz_new_bbox_device (ctx, &rect);
                        pdf_page_transform (ctx, page, &mediabox, &page_ctm);
                        ctm = fz_invert_matrix (page_ctm);
                        pdf_run_page (ctx, page, dev, fz_identity, NULL);
                        fz_close_device (ctx, dev);
                        fz_drop_device (ctx, dev);

                        rect.x0 += state.trimfuzz.x0;
                        rect.x1 += state.trimfuzz.x1;
                        rect.y0 += state.trimfuzz.y0;
                        rect.y1 += state.trimfuzz.y1;
                        rect = fz_transform_rect (rect, ctm);
                        rect = fz_intersect_rect (rect, mediabox);

                        if (!fz_is_empty_rect (rect)) {
                            mediabox = rect;
                        }

                        obj = pdf_new_array (ctx, pdf, 4);
                        pdf_array_push (ctx, obj,
                                        pdf_new_real (ctx, mediabox.x0));
                        pdf_array_push (ctx, obj,
                                        pdf_new_real (ctx, mediabox.y0));
                        pdf_array_push (ctx, obj,
                                        pdf_new_real (ctx, mediabox.x1));
                        pdf_array_push (ctx, obj,
                                        pdf_new_real (ctx, mediabox.y1));
                        pdf_dict_puts (ctx, pageobj, "llpp.TrimBox", obj);
                    }
                    else {
                        mediabox.x0 = pdf_to_real (ctx,
                                                   pdf_array_get (ctx, obj, 0));
                        mediabox.y0 = pdf_to_real (ctx,
                                                   pdf_array_get (ctx, obj, 1));
                        mediabox.x1 = pdf_to_real (ctx,
                                                   pdf_array_get (ctx, obj, 2));
                        mediabox.y1 = pdf_to_real (ctx,
                                                   pdf_array_get (ctx, obj, 3));
                    }

                    fz_drop_page (ctx, &page->super);
                    show = trim ? pageno % 5 == 0 : pageno % 20 == 0;
                    if (show) {
                        printd ("progress %f Trimming %d",
                                (double) (pageno + 1) / state.pagecount,
                                pageno + 1);
                    }
                }
                fz_catch (ctx) {
                    printd ("emsg failed to load page %d", pageno);
                }
            }
            else {
                int empty = 0;
                fz_rect cropbox;

                mediabox =
                    pdf_to_rect (ctx, pdf_dict_gets (ctx, pageobj, "MediaBox"));
                if (fz_is_empty_rect (mediabox)) {
                    mediabox.x0 = 0;
                    mediabox.y0 = 0;
                    mediabox.x1 = 612;
                    mediabox.y1 = 792;
                    empty = 1;
                }

                cropbox =
                    pdf_to_rect (ctx, pdf_dict_gets (ctx, pageobj, "CropBox"));
                if (!fz_is_empty_rect (cropbox)) {
                    if (empty) {
                        mediabox = cropbox;
                    }
                    else {
                        mediabox = fz_intersect_rect (mediabox, cropbox);
                    }
                }
                else {
                    if (empty) {
                        if (fz_is_empty_rect (rootmediabox)) {
                            printd ("emsg cannot find page size for page %d",
                                    pageno);
                        }
                        else {
                            mediabox = rootmediabox;
                        }
                    }
                }
            }
        }
        else {
            if (state.trimmargins && trimw) {
                fz_page *page;

                fz_try (ctx) {
                    page = fz_load_page (ctx, state.doc, pageno);
                    mediabox = fz_bound_page (ctx, page);
                    if (state.trimmargins) {
                        fz_rect rect;
                        fz_device *dev;

                        dev = fz_new_bbox_device (ctx, &rect);
                        fz_run_page (ctx, page, dev, fz_identity, NULL);
                        fz_close_device (ctx, dev);
                        fz_drop_device (ctx, dev);

                        rect.x0 += state.trimfuzz.x0;
                        rect.x1 += state.trimfuzz.x1;
                        rect.y0 += state.trimfuzz.y0;
                        rect.y1 += state.trimfuzz.y1;
                        rect = fz_intersect_rect (rect, mediabox);

                        if (!fz_is_empty_rect (rect)) {
                            mediabox = rect;
                        }
                    }
                    fz_drop_page (ctx, page);
                }
                fz_catch (ctx) {
                }
                if (trimf) {
                    size_t n = fwrite (&mediabox, sizeof (mediabox), 1, trimf);
                    if (n - 1) {
                        err (1, "fwrite trim mediabox");
                    }
                }
            }
            else {
                if (trimf) {
                    size_t n = fread (&mediabox, sizeof (mediabox), 1, trimf);
                    if (n - 1) {
                        err (1, "fread trim mediabox %d", pageno);
                    }
                }
                else {
                    fz_page *page;
                    fz_try (ctx) {
                        page = fz_load_page (ctx, state.doc, pageno);
                        mediabox = fz_bound_page (ctx, page);
                        fz_drop_page (ctx, page);

                        show = !state.trimmargins && pageno % 20 == 0;
                        if (show) {
                            printd ("progress %f Gathering dimensions %d",
                                    (double) (pageno) / state.pagecount,
                                    pageno);
                        }
                    }
                    fz_catch (ctx) {
                        printd ("emsg failed to load page %d", pageno);
                    }
                }
            }
        }

        if (state.pagedimcount == 0
            || ((void) (p = &state.pagedims[state.pagedimcount-1])
                , p->rotate != rotate)
            || memcmp (&p->mediabox, &mediabox, sizeof (mediabox))) {
            size_t size;

            size = (state.pagedimcount + 1) * sizeof (*state.pagedims);
            state.pagedims = realloc (state.pagedims, size);
            if (!state.pagedims) {
                err (1, "realloc pagedims to %zu (%d elems)",
                     size, state.pagedimcount + 1);
            }

            p = &state.pagedims[state.pagedimcount++];
            p->rotate = rotate;
            p->mediabox = mediabox;
            p->pageno = pageno;
        }
    }
    state.trimanew = 0;
    if (trimf) {
        if (fclose (trimf)) {
            err (1, "fclose");
        }
    }
}

static void layout (void)
{
    int pindex;
    fz_rect box;
    fz_matrix ctm;
    struct pagedim *p = NULL;
    float zw, w, maxw = 0.0, zoom = 1.0;

    if (state.pagedimcount == 0) return;

    switch (state.fitmodel) {
    case FitProportional:
        for (pindex = 0; pindex < state.pagedimcount; ++pindex) {
            float x0, x1;

            p = &state.pagedims[pindex];
            box = fz_transform_rect (p->mediabox,
                                     fz_rotate (p->rotate + state.rotate));

            x0 = fz_min (box.x0, box.x1);
            x1 = fz_max (box.x0, box.x1);

            w = x1 - x0;
            maxw = fz_max (w, maxw);
            zoom = state.w / maxw;
        }
        break;

    case FitPage:
        maxw = state.w;
        break;

    case FitWidth:
        break;

    default:
        ARSERT (0 && state.fitmodel);
    }

    for (pindex = 0; pindex < state.pagedimcount; ++pindex) {
        p = &state.pagedims[pindex];
        ctm = fz_rotate (state.rotate);
        box = fz_transform_rect (p->mediabox,
                                 fz_rotate (p->rotate + state.rotate));
        w = box.x1 - box.x0;
        switch (state.fitmodel) {
        case FitProportional:
            p->left = (int) (((maxw - w) * zoom) / 2.f);
            break;
        case FitPage:
            {
                float zh, h;
                zw = maxw / w;
                h = box.y1 - box.y0;
                zh = state.h / h;
                zoom = fz_min (zw, zh);
                p->left = (int) ((maxw - (w * zoom)) / 2.f);
            }
            break;
        case FitWidth:
            p->left = 0;
            zoom = state.w / w;
            break;
        }

        p->zoomctm = fz_scale (zoom, zoom);
        ctm = fz_concat (p->zoomctm, ctm);

        p->pagebox = p->mediabox;
        p->pagebox = fz_transform_rect (p->pagebox, fz_rotate (p->rotate));
        p->pagebox.x1 -= p->pagebox.x0;
        p->pagebox.y1 -= p->pagebox.y0;
        p->pagebox.x0 = 0;
        p->pagebox.y0 = 0;
        p->bounds = fz_round_rect (fz_transform_rect (p->pagebox, ctm));
        p->ctm = ctm;

        ctm = fz_concat (fz_translate (0, -p->mediabox.y1),
                         fz_scale (zoom, -zoom));
        p->tctmready = 0;
    }

    do {
        int x0 = fz_mini (p->bounds.x0, p->bounds.x1);
        int y0 = fz_mini (p->bounds.y0, p->bounds.y1);
        int x1 = fz_maxi (p->bounds.x0, p->bounds.x1);
        int y1 = fz_maxi (p->bounds.y0, p->bounds.y1);
        int boundw = x1 - x0;
        int boundh = y1 - y0;

        printd ("pdim %d %d %d %d", p->pageno, boundw, boundh, p->left);
    } while (p-- != state.pagedims);
}

static struct pagedim *pdimofpageno (int pageno)
{
    struct pagedim *pdim = state.pagedims;

    for (int i = 0; i < state.pagedimcount; ++i) {
        if (state.pagedims[i].pageno > pageno)
            break;
        pdim = &state.pagedims[i];
    }
    return pdim;
}

static void recurse_outline (fz_outline *outline, int level)
{
    while (outline) {
        if (outline->page >= 0) {
            fz_point p = {.x = outline->x, .y = outline->y};
            struct pagedim *pdim = pdimofpageno (outline->page);
            int h = fz_maxi (fz_absi (pdim->bounds.y1 - pdim->bounds.y0), 0);
            p = fz_transform_point (p, pdim->ctm);
            printd ("o %d %d %d %d %s",
                    level, outline->page, (int) p.y, h, outline->title);
        }
        else {
            printd ("on %d %s", level, outline->title);
        }
        if (outline->down) {
            recurse_outline (outline->down, level + 1);
        }
        outline = outline->next;
    }
}

static void process_outline (void)
{
    fz_outline *outline;

    if (!state.needoutline || !state.pagedimcount) return;

    state.needoutline = 0;
    outline = fz_load_outline (state.ctx, state.doc);
    if (outline) {
        recurse_outline (outline, 0);
        fz_drop_outline (state.ctx, outline);
    }
}

static char *strofline (fz_stext_line *line)
{
    char *p;
    char utf8[10];
    fz_stext_char *ch;
    size_t size = 0, cap = 80;

    p = malloc (cap + 1);
    if (!p) return NULL;

    for (ch = line->first_char; ch; ch = ch->next) {
        int n = fz_runetochar (utf8, ch->c);
        if (size + n > cap) {
            cap *= 2;
            p = realloc (p, cap + 1);
            if (!p) return NULL;
        }

        memcpy (p + size, utf8, n);
        size += n;
    }
    p[size] = 0;
    return p;
}

static int matchline (regex_t *re, fz_stext_line *line,
                      int stop, int pageno, double start)
{
    int ret;
    char *p;
    regmatch_t rm;

    p = strofline (line);
    if (!p) return -1;

    ret = regexec (re, p, 1, &rm, 0);
    if (ret) {
        free (p);
        if (ret != REG_NOMATCH) {
            size_t size;
            char errbuf[80];
            size = regerror (ret, re, errbuf, sizeof (errbuf));
            printd ("msg regexec error `%.*s'",
                    (int) size, errbuf);
            return -1;
        }
        return 0;
    }
    else {
        fz_quad s, e;
        fz_stext_char *ch;
        int o = 0;

        for (ch = line->first_char; ch; ch = ch->next) {
            o += fz_runelen (ch->c);
            if (o > rm.rm_so) {
                s = ch->quad;
                break;
            }
        }
        for (;ch; ch = ch->next) {
            o += fz_runelen (ch->c);
            if (o > rm.rm_eo) break;
        }
        e = ch->quad;

        if (!stop) {
            printd ("firstmatch %d %d %f %f %f %f %f %f %f %f",
                    pageno, 1,
                    s.ul.x, s.ul.y,
                    e.ur.x, s.ul.y,
                    e.lr.x, e.lr.y,
                    s.ul.x, e.lr.y);

            printd ("progress 1 found at %d `%.*s' in %f sec",
                    pageno + 1, (int) (rm.rm_eo - rm.rm_so), &p[rm.rm_so],
                    now () - start);
        }
        else {
            printd ("match %d %d %f %f %f %f %f %f %f %f",
                    pageno, 2,
                    s.ul.x, s.ul.y,
                    e.ur.x, s.ul.y,
                    e.lr.x, e.lr.y,
                    s.ul.x, e.lr.y);
        }
        free (p);
        return 1;
    }
}

/* wishful thinking function */
static void search (regex_t *re, int pageno, int y, int forward)
{
    fz_device *tdev;
    fz_stext_page *text;
    struct pagedim *pdim;
    int stop = 0, niters = 0;
    double start, end;
    fz_page *page;
    fz_stext_block *block;

    start = now ();
    while (pageno >= 0 && pageno < state.pagecount && !stop) {
        if (niters++ == 5) {
            niters = 0;
            if (hasdata ()) {
                printd ("progress 1 attention requested aborting search at %d",
                        pageno);
                stop = 1;
            }
            else {
                printd ("progress %f searching in page %d",
                        (double) (pageno + 1) / state.pagecount,
                        pageno);
            }
        }
        pdim = pdimofpageno (pageno);
        text = fz_new_stext_page (state.ctx, pdim->mediabox);
        tdev = fz_new_stext_device (state.ctx, text, 0);

        page = fz_load_page (state.ctx, state.doc, pageno);
        fz_run_page (state.ctx, page, tdev, pagectm1 (page, pdim), NULL);

        fz_close_device (state.ctx, tdev);
        fz_drop_device (state.ctx, tdev);

        if (forward) {
            for (block = text->first_block; block; block = block->next) {
                fz_stext_line *line;

                if (block->type != FZ_STEXT_BLOCK_TEXT) continue;
                for (line = block->u.t.first_line; line; line = line->next) {
                    if (line->bbox.y0 < y + 1) continue;

                    switch (matchline (re, line, stop, pageno, start)) {
                    case 0: break;
                    case 1: stop = 1; break;
                    case -1: stop = 1; goto endloop;
                    }
                }
            }
        }
        else {
            for (block = text->last_block; block; block = block->prev) {
                fz_stext_line *line;

                if (block->type != FZ_STEXT_BLOCK_TEXT) continue;
                for (line = block->u.t.last_line; line; line = line->prev) {
                    if (line->bbox.y0 < y + 1) continue;

                    switch (matchline (re, line, stop, pageno, start)) {
                    case 0: break;
                    case 1: stop = 1; break;
                    case -1: stop = 1; goto endloop;
                    }
                }
            }
        }

        if (forward) {
            pageno += 1;
            y = 0;
        }
        else {
            pageno -= 1;
            y = INT_MAX;
        }
    endloop:
        fz_drop_stext_page (state.ctx, text);
        fz_drop_page (state.ctx, page);
    }
    end = now ();
    if (!stop) {
        printd ("progress 1 no matches %f sec", end - start);
    }
    printd ("clearrects");
}

static void set_tex_params (int colorspace)
{
    switch (colorspace) {
    case 0:
        state.texiform = GL_RGBA8;
        state.texform = GL_RGBA;
        state.texty = GL_UNSIGNED_BYTE;
        state.colorspace = fz_device_rgb (state.ctx);
        break;
    case 1:
        state.texiform = GL_LUMINANCE_ALPHA;
        state.texform = GL_LUMINANCE_ALPHA;
        state.texty = GL_UNSIGNED_BYTE;
        state.colorspace = fz_device_gray (state.ctx);
        break;
    default:
        errx (1, "invalid colorspce %d", colorspace);
    }
}

static void realloctexts (int texcount)
{
    size_t size;

    if (texcount == state.texcount) return;

    if (texcount < state.texcount) {
        glDeleteTextures (state.texcount - texcount,
                          state.texids + texcount);
    }

    size = texcount * (sizeof (*state.texids) + sizeof (*state.texowners));
    state.texids = realloc (state.texids, size);
    if (!state.texids) {
        err (1, "realloc texs %zu", size);
    }

    state.texowners = (void *) (state.texids + texcount);
    if (texcount > state.texcount) {
        glGenTextures (texcount - state.texcount,
                       state.texids + state.texcount);
        for (int i = state.texcount; i < texcount; ++i) {
            state.texowners[i].w = -1;
            state.texowners[i].slice = NULL;
        }
    }
    state.texcount = texcount;
    state.texindex = 0;
}

static char *mbtoutf8 (char *s)
{
    char *p, *r;
    wchar_t *tmp;
    size_t i, ret, len;

    if (state.utf8cs) {
        return s;
    }

    len = mbstowcs (NULL, s, strlen (s));
    if (len == 0 || len == (size_t) -1) {
        if (len)
            printd ("emsg mbtoutf8: mbstowcs: %d:%s", errno, strerror (errno));
        return s;
    }

    tmp = calloc (len, sizeof (wchar_t));
    if (!tmp) {
        printd ("emsg mbtoutf8: calloc(%zu, %zu): %d:%s",
                len, sizeof (wchar_t), errno, strerror (errno));
        return s;
    }

    ret = mbstowcs (tmp, s, len);
    if (ret == (size_t) -1) {
        printd ("emsg mbtoutf8: mbswcs %zu characters failed: %d:%s",
                len, errno, strerror (errno));
        free (tmp);
        return s;
    }

    len = 0;
    for (i = 0; i < ret; ++i) {
        len += fz_runelen (tmp[i]);
    }

    p = r = malloc (len + 1);
    if (!r) {
        printd ("emsg mbtoutf8: malloc(%zu)", len);
        free (tmp);
        return s;
    }

    for (i = 0; i < ret; ++i) {
        p += fz_runetochar (p, tmp[i]);
    }
    *p = 0;
    free (tmp);
    return r;
}

value ml_mbtoutf8 (value s_v);
value ml_mbtoutf8 (value s_v)
{
    CAMLparam1 (s_v);
    CAMLlocal1 (ret_v);
    char *s, *r;

    s = String_val (s_v);
    r = mbtoutf8 (s);
    if (r == s) {
        ret_v = s_v;
    }
    else {
        ret_v = caml_copy_string (r);
        free (r);
    }
    CAMLreturn (ret_v);
}

static void * mainloop (void UNUSED_ATTR *unused)
{
    char *p = NULL;
    int len, ret, oldlen = 0;

    fz_var (p);
    fz_var (oldlen);
    for (;;) {
        len = readlen (state.csock);
        if (len == 0) {
            errx (1, "readlen returned 0");
        }

        if (oldlen < len + 1) {
            p = realloc (p, len + 1);
            if (!p) {
                err (1, "realloc %d failed", len + 1);
            }
            oldlen = len + 1;
        }
        readdata (state.csock, p, len);
        p[len] = 0;

        if (!strncmp ("open", p, 4)) {
            int off, usedoccss, ok = 0, layouth;
            char *password;
            char *filename;
            char *utf8filename;
            size_t filenamelen;

            fz_var (ok);
            ret = sscanf (p + 5, " %d %d %n", &usedoccss, &layouth, &off);
            if (ret != 2) {
                errx (1, "malformed open `%.*s' ret=%d", len, p, ret);
            }

            filename = p + 5 + off;
            filenamelen = strlen (filename);
            password = filename + filenamelen + 1;

            if (password[strlen (password) + 1]) {
                fz_set_user_css (state.ctx, password + strlen (password) + 1);
            }

            lock ("open");
            fz_set_use_document_css (state.ctx, usedoccss);
            fz_try (state.ctx) {
                ok = openxref (filename, password, layouth);
            }
            fz_catch (state.ctx) {
                utf8filename = mbtoutf8 (filename);
                printd ("emsg Error loading %s: %s",
                        utf8filename, fz_caught_message (state.ctx));
                if (utf8filename != filename) {
                    free (utf8filename);
                }
            }
            if (ok) {
                docinfo ();
                initpdims ();
            }
            unlock ("open");
            state.needoutline = ok;
        }
        else if (!strncmp ("cs", p, 2)) {
            int i, colorspace;

            ret = sscanf (p + 2, " %d", &colorspace);
            if (ret != 1) {
                errx (1, "malformed cs `%.*s' ret=%d", len, p, ret);
            }
            lock ("cs");
            set_tex_params (colorspace);
            for (i = 0; i < state.texcount; ++i) {
                state.texowners[i].w = -1;
                state.texowners[i].slice = NULL;
            }
            unlock ("cs");
        }
        else if (!strncmp ("freepage", p, 8)) {
            void *ptr;

            ret = sscanf (p + 8, " %" SCNxPTR, (uintptr_t *) &ptr);
            if (ret != 1) {
                errx (1, "malformed freepage `%.*s' ret=%d", len, p, ret);
            }
            lock ("freepage");
            freepage (ptr);
            unlock ("freepage");
        }
        else if (!strncmp ("freetile", p, 8)) {
            void *ptr;

            ret = sscanf (p + 8, " %" SCNxPTR, (uintptr_t *) &ptr);
            if (ret != 1) {
                errx (1, "malformed freetile `%.*s' ret=%d", len, p, ret);
            }
            lock ("freetile");
            freetile (ptr);
            unlock ("freetile");
        }
        else if (!strncmp ("search", p, 6)) {
            int icase, pageno, y, len2, forward;
            char *pattern;
            regex_t re;

            ret = sscanf (p + 6, " %d %d %d %d,%n",
                          &icase, &pageno, &y, &forward, &len2);
            if (ret != 4) {
                errx (1, "malformed search `%s' ret=%d", p, ret);
            }

            pattern = p + 6 + len2;
            ret = regcomp (&re, pattern,
                           REG_EXTENDED | (icase ? REG_ICASE : 0));
            if (ret) {
                char errbuf[80];
                size_t size;

                size = regerror (ret, &re, errbuf, sizeof (errbuf));
                printd ("msg regcomp failed `%.*s'", (int) size, errbuf);
            }
            else {
                lock ("search");
                search (&re, pageno, y, forward);
                unlock ("search");
                regfree (&re);
            }
        }
        else if (!strncmp ("geometry", p, 8)) {
            int w, h, fitmodel;

            printd ("clear");
            ret = sscanf (p + 8, " %d %d %d", &w, &h, &fitmodel);
            if (ret != 3) {
                errx (1, "malformed geometry `%.*s' ret=%d", len, p, ret);
            }

            lock ("geometry");
            state.h = h;
            if (w != state.w) {
                state.w = w;
                for (int i = 0; i < state.texcount; ++i) {
                    state.texowners[i].slice = NULL;
                }
            }
            state.fitmodel = fitmodel;
            layout ();
            process_outline ();

            state.gen++;
            unlock ("geometry");
            printd ("continue %d", state.pagecount);
        }
        else if (!strncmp ("reqlayout", p, 9)) {
            char *nameddest;
            int rotate, off, h;
            int fitmodel;
            pdf_document *pdf;

            printd ("clear");
            ret = sscanf (p + 9, " %d %d %d %n",
                          &rotate, &fitmodel, &h, &off);
            if (ret != 3) {
                errx (1, "bad reqlayout line `%.*s' ret=%d", len, p, ret);
            }
            lock ("reqlayout");
            pdf = pdf_specifics (state.ctx, state.doc);
            if (state.rotate != rotate || state.fitmodel != fitmodel) {
                state.gen += 1;
            }
            state.rotate = rotate;
            state.fitmodel = fitmodel;
            state.h = h;
            layout ();
            process_outline ();

            nameddest = p + 9 + off;
            if (pdf && nameddest && *nameddest) {
                fz_point xy;
                struct pagedim *pdim;
                int pageno = pdf_lookup_anchor (state.ctx, pdf, nameddest,
                                                &xy.x, &xy.y);
                pdim = pdimofpageno (pageno);
                xy = fz_transform_point (xy, pdim->ctm);
                printd ("a %d %d %d", pageno, (int) xy.x, (int) xy.y);
            }

            state.gen++;
            unlock ("reqlayout");
            printd ("continue %d", state.pagecount);
        }
        else if (!strncmp ("page", p, 4)) {
            double a, b;
            struct page *page;
            int pageno, pindex;

            ret = sscanf (p + 4, " %d %d", &pageno, &pindex);
            if (ret != 2) {
                errx (1, "bad page line `%.*s' ret=%d", len, p, ret);
            }

            lock ("page");
            a = now ();
            page = loadpage (pageno, pindex);
            b = now ();
            unlock ("page");

            printd ("page %" PRIxPTR " %f", (uintptr_t) page, b - a);
        }
        else if (!strncmp ("tile", p, 4)) {
            int x, y, w, h;
            struct page *page;
            struct tile *tile;
            double a, b;
            void *data;

            ret = sscanf (p + 4, " %" SCNxPTR " %d %d %d %d %" SCNxPTR,
                          (uintptr_t *) &page, &x, &y, &w, &h,
                          (uintptr_t *) &data);
            if (ret != 6) {
                errx (1, "bad tile line `%.*s' ret=%d", len, p, ret);
            }

            lock ("tile");
            a = now ();
            tile = rendertile (page, x, y, w, h, data);
            b = now ();
            unlock ("tile");

            printd ("tile %d %d %" PRIxPTR " %u %f",
                    x, y, (uintptr_t) (tile),
                    tile->w * tile->h * tile->pixmap->n,
                    b - a);
        }
        else if (!strncmp ("trimset", p, 7)) {
            fz_irect fuzz;
            int trimmargins;

            ret = sscanf (p + 7, " %d %d %d %d %d",
                          &trimmargins, &fuzz.x0, &fuzz.y0, &fuzz.x1, &fuzz.y1);
            if (ret != 5) {
                errx (1, "malformed trimset `%.*s' ret=%d", len, p, ret);
            }
            lock ("trimset");
            state.trimmargins = trimmargins;
            if (memcmp (&fuzz, &state.trimfuzz, sizeof (fuzz))) {
                state.trimanew = 1;
                state.trimfuzz = fuzz;
            }
            unlock ("trimset");
        }
        else if (!strncmp ("settrim", p, 7)) {
            fz_irect fuzz;
            int trimmargins;

            ret = sscanf (p + 7, " %d %d %d %d %d",
                          &trimmargins, &fuzz.x0, &fuzz.y0, &fuzz.x1, &fuzz.y1);
            if (ret != 5) {
                errx (1, "malformed settrim `%.*s' ret=%d", len, p, ret);
            }
            printd ("clear");
            lock ("settrim");
            state.trimmargins = trimmargins;
            state.needoutline = 1;
            if (memcmp (&fuzz, &state.trimfuzz, sizeof (fuzz))) {
                state.trimanew = 1;
                state.trimfuzz = fuzz;
            }
            state.pagedimcount = 0;
            free (state.pagedims);
            state.pagedims = NULL;
            initpdims ();
            layout ();
            process_outline ();
            unlock ("settrim");
            printd ("continue %d", state.pagecount);
        }
        else if (!strncmp ("sliceh", p, 6)) {
            int h;

            ret = sscanf (p + 6, " %d", &h);
            if (ret != 1) {
                errx (1, "malformed sliceh `%.*s' ret=%d", len, p, ret);
            }
            if (h != state.sliceheight) {
                state.sliceheight = h;
                for (int i = 0; i < state.texcount; ++i) {
                    state.texowners[i].w = -1;
                    state.texowners[i].h = -1;
                    state.texowners[i].slice = NULL;
                }
            }
        }
        else if (!strncmp ("interrupt", p, 9)) {
            printd ("vmsg interrupted");
        }
        else {
            errx (1, "unknown command %.*s", len, p);
        }
    }
    return 0;
}

value ml_isexternallink (value uri_v);
value ml_isexternallink (value uri_v)
{
    CAMLparam1 (uri_v);
    int ext = fz_is_external_link (state.ctx, String_val (uri_v));
    CAMLreturn (Val_bool (ext));
}

value ml_uritolocation (value uri_v);
value ml_uritolocation (value uri_v)
{
    CAMLparam1 (uri_v);
    CAMLlocal1 (ret_v);
    int pageno;
    fz_point xy;
    struct pagedim *pdim;

    pageno = fz_resolve_link (state.ctx, state.doc, String_val (uri_v),
                              &xy.x, &xy.y);
    pdim = pdimofpageno (pageno);
    xy = fz_transform_point (xy, pdim->ctm);
    ret_v = caml_alloc_tuple (3);
    Field (ret_v, 0) = Val_int (pageno);
    Field (ret_v, 1) = caml_copy_double ((double) xy.x);
    Field (ret_v, 2) = caml_copy_double ((double) xy.y);
    CAMLreturn (ret_v);
}

value ml_realloctexts (value texcount_v);
value ml_realloctexts (value texcount_v)
{
    CAMLparam1 (texcount_v);
    int ok;

    if (trylock (__func__)) {
        ok = 0;
        goto done;
    }
    realloctexts (Int_val (texcount_v));
    ok = 1;
    unlock (__func__);

 done:
    CAMLreturn (Val_bool (ok));
}

static void recti (int x0, int y0, int x1, int y1)
{
    GLfloat *v = state.vertices;

    glVertexPointer (2, GL_FLOAT, 0, v);
    v[0] = x0; v[1] = y0;
    v[2] = x1; v[3] = y0;
    v[4] = x0; v[5] = y1;
    v[6] = x1; v[7] = y1;
    glDrawArrays (GL_TRIANGLE_STRIP, 0, 4);
}

static void showsel (struct page *page, int ox, int oy)
{
    fz_irect bbox;
    fz_rect rect;
    fz_stext_block *block;
    int seen = 0;
    unsigned char selcolor[] = {15,15,15,140};

    if (!page->fmark || !page->lmark) return;

    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_SRC_ALPHA);
    glColor4ubv (selcolor);

    ox += state.pagedims[page->pdimno].bounds.x0;
    oy += state.pagedims[page->pdimno].bounds.y0;

    for (block = page->text->first_block; block; block = block->next) {
        fz_stext_line *line;

        if (block->type != FZ_STEXT_BLOCK_TEXT) continue;
        for (line = block->u.t.first_line; line; line = line->next) {
            fz_stext_char *ch;

            rect = fz_empty_rect;
            for (ch = line->first_char; ch; ch = ch->next) {
                fz_rect r;
                if (ch == page->fmark) seen = 1;
                r = fz_rect_from_quad (ch->quad);
                if (seen) rect = fz_union_rect (rect, r);
                if (ch == page->lmark) {
                    bbox = fz_round_rect (rect);
                    recti (bbox.x0 + ox, bbox.y0 + oy,
                           bbox.x1 + ox, bbox.y1 + oy);
                    goto done;
                }
            }
            bbox = fz_round_rect (rect);
            recti (bbox.x0 + ox, bbox.y0 + oy,
                   bbox.x1 + ox, bbox.y1 + oy);
        }
    }
done:
    glDisable (GL_BLEND);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include "glfont.c"
#pragma GCC diagnostic pop

static void stipplerect (fz_matrix m,
                         fz_point p1,
                         fz_point p2,
                         fz_point p3,
                         fz_point p4,
                         GLfloat *texcoords,
                         GLfloat *vertices)
{
    p1 = fz_transform_point (p1, m);
    p2 = fz_transform_point (p2, m);
    p3 = fz_transform_point (p3, m);
    p4 = fz_transform_point (p4, m);
    {
        float w, h, s, t;

        w = p2.x - p1.x;
        h = p2.y - p1.y;
        t = hypotf (w, h) * .25f;

        w = p3.x - p2.x;
        h = p3.y - p2.y;
        s = hypotf (w, h) * .25f;

        texcoords[0] = 0; vertices[0] = p1.x; vertices[1] = p1.y;
        texcoords[1] = t; vertices[2] = p2.x; vertices[3] = p2.y;

        texcoords[2] = 0; vertices[4] = p2.x; vertices[5] = p2.y;
        texcoords[3] = s; vertices[6] = p3.x; vertices[7] = p3.y;

        texcoords[4] = 0; vertices[8] = p3.x; vertices[9] = p3.y;
        texcoords[5] = t; vertices[10] = p4.x; vertices[11] = p4.y;

        texcoords[6] = 0; vertices[12] = p4.x; vertices[13] = p4.y;
        texcoords[7] = s; vertices[14] = p1.x; vertices[15] = p1.y;
    }
    glDrawArrays (GL_LINES, 0, 8);
}

static void solidrect (fz_matrix m,
                       fz_point p1,
                       fz_point p2,
                       fz_point p3,
                       fz_point p4,
                       GLfloat *vertices)
{
    p1 = fz_transform_point (p1, m);
    p2 = fz_transform_point (p2, m);
    p3 = fz_transform_point (p3, m);
    p4 = fz_transform_point (p4, m);
    vertices[0] = p1.x; vertices[1] = p1.y;
    vertices[2] = p2.x; vertices[3] = p2.y;

    vertices[4] = p3.x; vertices[5] = p3.y;
    vertices[6] = p4.x; vertices[7] = p4.y;
    glDrawArrays (GL_TRIANGLE_FAN, 0, 4);
}

static void ensurelinks (struct page *page)
{
    if (!page->links)
        page->links = fz_load_links (state.ctx, page->fzpage);
}

static void highlightlinks (struct page *page, int xoff, int yoff)
{
    fz_matrix ctm;
    fz_link *link;
    GLfloat *texcoords = state.texcoords;
    GLfloat *vertices = state.vertices;

    ensurelinks (page);

    glEnable (GL_TEXTURE_1D);
    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBindTexture (GL_TEXTURE_1D, state.stid);

    xoff -= state.pagedims[page->pdimno].bounds.x0;
    yoff -= state.pagedims[page->pdimno].bounds.y0;
    ctm = fz_concat (pagectm (page), fz_translate (xoff, yoff));

    glTexCoordPointer (1, GL_FLOAT, 0, texcoords);
    glVertexPointer (2, GL_FLOAT, 0, vertices);

    for (link = page->links; link; link = link->next) {
        fz_point p1, p2, p3, p4;

        p1.x = link->rect.x0;
        p1.y = link->rect.y0;

        p2.x = link->rect.x1;
        p2.y = link->rect.y0;

        p3.x = link->rect.x1;
        p3.y = link->rect.y1;

        p4.x = link->rect.x0;
        p4.y = link->rect.y1;

        /* TODO: different colours for different schemes */
        if (fz_is_external_link (state.ctx, link->uri)) glColor3ub (0, 0, 255);
        else glColor3ub (255, 0, 0);

        stipplerect (ctm, p1, p2, p3, p4, texcoords, vertices);
    }

    for (int i = 0; i < page->annotcount; ++i) {
        fz_point p1, p2, p3, p4;
        struct annot *annot = &page->annots[i];

        p1.x = annot->bbox.x0;
        p1.y = annot->bbox.y0;

        p2.x = annot->bbox.x1;
        p2.y = annot->bbox.y0;

        p3.x = annot->bbox.x1;
        p3.y = annot->bbox.y1;

        p4.x = annot->bbox.x0;
        p4.y = annot->bbox.y1;

        glColor3ub (0, 0, 128);
        stipplerect (ctm, p1, p2, p3, p4, texcoords, vertices);
    }

    glDisable (GL_BLEND);
    glDisable (GL_TEXTURE_1D);
}

static int compareslinks (const void *l, const void *r)
{
    struct slink const *ls = l;
    struct slink const *rs = r;
    if (ls->bbox.y0 == rs->bbox.y0) {
        return rs->bbox.x0 - rs->bbox.x0;
    }
    return ls->bbox.y0 - rs->bbox.y0;
}

static void droptext (struct page *page)
{
    if (page->text) {
        fz_drop_stext_page (state.ctx, page->text);
        page->fmark = NULL;
        page->lmark = NULL;
        page->text = NULL;
    }
}

static void dropannots (struct page *page)
{
    if (page->annots) {
        free (page->annots);
        page->annots = NULL;
        page->annotcount = 0;
    }
}

static void ensureannots (struct page *page)
{
    int i, count = 0;
    size_t annotsize = sizeof (*page->annots);
    pdf_annot *annot;
    pdf_document *pdf;
    pdf_page *pdfpage;

    pdf = pdf_specifics (state.ctx, state.doc);
    if (!pdf) return;

    pdfpage = pdf_page_from_fz_page (state.ctx, page->fzpage);
    if (state.gen != page->agen) {
        dropannots (page);
        page->agen = state.gen;
    }
    if (page->annots) return;

    for (annot = pdf_first_annot (state.ctx, pdfpage);
         annot;
         annot = pdf_next_annot (state.ctx, annot)) {
        count++;
    }

    if (count > 0) {
        page->annotcount = count;
        page->annots = calloc (count, annotsize);
        if (!page->annots) {
            err (1, "calloc annots %d", count);
        }

        for (annot = pdf_first_annot (state.ctx, pdfpage), i = 0;
             annot;
             annot = pdf_next_annot (state.ctx, annot), i++) {
            fz_rect rect;

            rect = pdf_bound_annot (state.ctx, annot);
            page->annots[i].annot = annot;
            page->annots[i].bbox = fz_round_rect (rect);
        }
    }
}

static void dropslinks (struct page *page)
{
    if (page->slinks) {
        free (page->slinks);
        page->slinks = NULL;
        page->slinkcount = 0;
    }
    if (page->links) {
        fz_drop_link (state.ctx, page->links);
        page->links = NULL;
    }
}

static void ensureslinks (struct page *page)
{
    fz_matrix ctm;
    int i, count;
    size_t slinksize = sizeof (*page->slinks);
    fz_link *link;

    ensureannots (page);
    if (state.gen != page->sgen) {
        dropslinks (page);
        page->sgen = state.gen;
    }
    if (page->slinks) return;

    ensurelinks (page);
    ctm = pagectm (page);

    count = page->annotcount;
    for (link = page->links; link; link = link->next) {
        count++;
    }
    if (count > 0) {
        int j;

        page->slinkcount = count;
        page->slinks = calloc (count, slinksize);
        if (!page->slinks) {
            err (1, "calloc slinks %d", count);
        }

        for (i = 0, link = page->links; link; ++i, link = link->next) {
            fz_rect rect;

            rect = link->rect;
            rect = fz_transform_rect (rect, ctm);
            page->slinks[i].tag = SLINK;
            page->slinks[i].u.link = link;
            page->slinks[i].bbox = fz_round_rect (rect);
        }
        for (j = 0; j < page->annotcount; ++j, ++i) {
            fz_rect rect;
            rect = pdf_bound_annot (state.ctx, page->annots[j].annot);
            rect = fz_transform_rect (rect, ctm);
            page->slinks[i].bbox = fz_round_rect (rect);

            page->slinks[i].tag = SANNOT;
            page->slinks[i].u.annot = page->annots[j].annot;
        }
        qsort (page->slinks, count, slinksize, compareslinks);
    }
}

static void highlightslinks (struct page *page, int xoff, int yoff,
                             int noff, char *targ, mlsize_t tlen, int hfsize)
{
    char buf[40];
    struct slink *slink;
    float x0, y0, x1, y1, w;

    ensureslinks (page);
    glColor3ub (0xc3, 0xb0, 0x91);
    for (int i = 0; i < page->slinkcount; ++i) {
        fmt_linkn (buf, i + noff);
        if (!tlen || !strncmp (targ, buf, tlen)) {
            slink = &page->slinks[i];

            x0 = slink->bbox.x0 + xoff - 5;
            y1 = slink->bbox.y0 + yoff - 5;
            y0 = y1 + 10 + hfsize;
            w = measure_string (state.face, hfsize, buf);
            x1 = x0 + w + 10;
            recti ((int) x0, (int) y0, (int) x1, (int) y1);
        }
    }

    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable (GL_TEXTURE_2D);
    glColor3ub (0, 0, 0);
    for (int i = 0; i < page->slinkcount; ++i) {
        fmt_linkn (buf, i + noff);
        if (!tlen || !strncmp (targ, buf, tlen)) {
            slink = &page->slinks[i];

            x0 = slink->bbox.x0 + xoff;
            y0 = slink->bbox.y0 + yoff + hfsize;
            draw_string (state.face, hfsize, x0, y0, buf);
        }
    }
    glDisable (GL_TEXTURE_2D);
    glDisable (GL_BLEND);
}

static void uploadslice (struct tile *tile, struct slice *slice)
{
    int offset;
    struct slice *slice1;
    unsigned char *texdata;

    offset = 0;
    for (slice1 = tile->slices; slice != slice1; slice1++) {
        offset += slice1->h * tile->w * tile->pixmap->n;
    }
    if (slice->texindex != -1 && slice->texindex < state.texcount
        && state.texowners[slice->texindex].slice == slice) {
        glBindTexture (TEXT_TYPE, state.texids[slice->texindex]);
    }
    else {
        int subimage = 0;
        int texindex = state.texindex++ % state.texcount;

        if (state.texowners[texindex].w == tile->w) {
            if (state.texowners[texindex].h >= slice->h) {
                subimage = 1;
            }
            else {
                state.texowners[texindex].h = slice->h;
            }
        }
        else {
            state.texowners[texindex].h = slice->h;
        }

        state.texowners[texindex].w = tile->w;
        state.texowners[texindex].slice = slice;
        slice->texindex = texindex;

        glBindTexture (TEXT_TYPE, state.texids[texindex]);
#if TEXT_TYPE == GL_TEXTURE_2D
        glTexParameteri (TEXT_TYPE, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri (TEXT_TYPE, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri (TEXT_TYPE, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri (TEXT_TYPE, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
#endif
        if (tile->pbo) {
            state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, tile->pbo->id);
            texdata = 0;
        }
        else {
            texdata = tile->pixmap->samples;
        }
        if (subimage) {
            glTexSubImage2D (TEXT_TYPE,
                             0,
                             0,
                             0,
                             tile->w,
                             slice->h,
                             state.texform,
                             state.texty,
                             texdata+offset
                );
        }
        else {
            glTexImage2D (TEXT_TYPE,
                          0,
                          state.texiform,
                          tile->w,
                          slice->h,
                          0,
                          state.texform,
                          state.texty,
                          texdata+offset
                );
        }
        if (tile->pbo) {
            state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, 0);
        }
    }
}

void ml_begintiles (void);
void ml_begintiles (void)
{
    glEnable (TEXT_TYPE);
    glTexCoordPointer (2, GL_FLOAT, 0, state.texcoords);
    glVertexPointer (2, GL_FLOAT, 0, state.vertices);
}

void ml_endtiles (void);
void ml_endtiles (void)
{
    glDisable (TEXT_TYPE);
}

void ml_drawtile (value args_v, value ptr_v);
void ml_drawtile (value args_v, value ptr_v)
{
    CAMLparam2 (args_v, ptr_v);
    int dispx = Int_val (Field (args_v, 0));
    int dispy = Int_val (Field (args_v, 1));
    int dispw = Int_val (Field (args_v, 2));
    int disph = Int_val (Field (args_v, 3));
    int tilex = Int_val (Field (args_v, 4));
    int tiley = Int_val (Field (args_v, 5));
    char *s = String_val (ptr_v);
    struct tile *tile = parse_pointer (__func__, s);
    int slicey, firstslice;
    struct slice *slice;
    GLfloat *texcoords = state.texcoords;
    GLfloat *vertices = state.vertices;

    firstslice = tiley / tile->sliceheight;
    slice = &tile->slices[firstslice];
    slicey = tiley % tile->sliceheight;

    while (disph > 0) {
        int dh;

        dh = slice->h - slicey;
        dh = fz_mini (disph, dh);
        uploadslice (tile, slice);

        texcoords[0] = tilex;       texcoords[1] = slicey;
        texcoords[2] = tilex+dispw; texcoords[3] = slicey;
        texcoords[4] = tilex;       texcoords[5] = slicey+dh;
        texcoords[6] = tilex+dispw; texcoords[7] = slicey+dh;

        vertices[0] = dispx;        vertices[1] = dispy;
        vertices[2] = dispx+dispw;  vertices[3] = dispy;
        vertices[4] = dispx;        vertices[5] = dispy+dh;
        vertices[6] = dispx+dispw;  vertices[7] = dispy+dh;

#if TEXT_TYPE == GL_TEXTURE_2D
        for (int i = 0; i < 8; ++i) {
            texcoords[i] /= ((i & 1) == 0 ? tile->w : slice->h);
        }
#endif

        glDrawArrays (GL_TRIANGLE_STRIP, 0, 4);
        dispy += dh;
        disph -= dh;
        slice++;
        ARSERT (!(slice - tile->slices >= tile->slicecount && disph > 0));
        slicey = 0;
    }
    CAMLreturn0;
}

static void drawprect (struct page *page, int xoff, int yoff, value rects_v)
{
    fz_matrix ctm;
    fz_point p1, p2, p3, p4;
    GLfloat *vertices = state.vertices;

    xoff -= state.pagedims[page->pdimno].bounds.x0;
    yoff -= state.pagedims[page->pdimno].bounds.y0;
    ctm = fz_concat (pagectm (page), fz_translate (xoff, yoff));

    glEnable (GL_BLEND);
    glVertexPointer (2, GL_FLOAT, 0, vertices);

    glColor4d (
        Double_array_field (rects_v, 0),
        Double_array_field (rects_v, 1),
        Double_array_field (rects_v, 2),
        Double_array_field (rects_v, 3)
        );
    p1.x = (float) Double_array_field (rects_v, 4);
    p1.y = (float) Double_array_field (rects_v, 5);

    p2.x = (float) Double_array_field (rects_v, 6);
    p2.y = p1.y;

    p3.x = p2.x;
    p3.y = (float) Double_array_field (rects_v, 7);

    p4.x = p1.x;
    p4.y = p3.y;
    solidrect (ctm, p1, p2, p3, p4, vertices);
    glDisable (GL_BLEND);
}

value ml_postprocess (value ptr_v, value hlinks_v,
                      value xoff_v, value yoff_v,
                      value li_v);
value ml_postprocess (value ptr_v, value hlinks_v,
                      value xoff_v, value yoff_v,
                      value li_v)
{
    CAMLparam5 (ptr_v, hlinks_v, xoff_v, yoff_v, li_v);
    int xoff = Int_val (xoff_v);
    int yoff = Int_val (yoff_v);
    int noff = Int_val (Field (li_v, 0));
    char *targ = String_val (Field (li_v, 1));
    mlsize_t tlen = caml_string_length (Field (li_v, 1));
    int hfsize = Int_val (Field (li_v, 2));
    char *s = String_val (ptr_v);
    int hlmask = Int_val (hlinks_v);
    struct page *page = parse_pointer (__func__, s);

    if (!page->fzpage) {
        /* deal with loadpage failed pages */
        goto done;
    }

    if (trylock (__func__)) {
        noff = -1;
        goto done;
    }

    ensureannots (page);
    if (hlmask & 1) highlightlinks (page, xoff, yoff);
    if (hlmask & 2) {
        highlightslinks (page, xoff, yoff, noff, targ, tlen, hfsize);
        noff = page->slinkcount;
    }
    if (page->tgen == state.gen) {
        showsel (page, xoff, yoff);
    }
    unlock (__func__);

 done:
    CAMLreturn (Val_int (noff));
}

void ml_drawprect (value ptr_v, value xoff_v, value yoff_v, value rects_v);
void ml_drawprect (value ptr_v, value xoff_v, value yoff_v, value rects_v)
{
    CAMLparam4 (ptr_v, xoff_v, yoff_v, rects_v);
    int xoff = Int_val (xoff_v);
    int yoff = Int_val (yoff_v);
    char *s = String_val (ptr_v);
    struct page *page = parse_pointer (__func__, s);

    drawprect (page, xoff, yoff, rects_v);
    CAMLreturn0;
}

static struct annot *getannot (struct page *page, int x, int y)
{
    fz_point p;
    fz_matrix ctm;
    const fz_matrix *tctm;
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    if (!page->annots) return NULL;

    if (pdf) {
        trimctm (pdf_page_from_fz_page (state.ctx, page->fzpage), page->pdimno);
        tctm = &state.pagedims[page->pdimno].tctm;
    }
    else {
        tctm = &fz_identity;
    }

    p.x = x;
    p.y = y;

    ctm = fz_concat (*tctm, state.pagedims[page->pdimno].ctm);
    ctm = fz_invert_matrix (ctm);
    p = fz_transform_point (p, ctm);

    if (pdf) {
        for (int i = 0; i < page->annotcount; ++i) {
            struct annot *a = &page->annots[i];
            fz_rect rect;

            rect = pdf_bound_annot (state.ctx, a->annot);
            if (p.x >= rect.x0 && p.x <= rect.x1) {
                if (p.y >= rect.y0 && p.y <= rect.y1)
                    return a;
            }
        }
    }
    return NULL;
}

static fz_link *getlink (struct page *page, int x, int y)
{
    fz_point p;
    fz_link *link;

    ensureslinks (page);

    p.x = x;
    p.y = y;

    p = fz_transform_point (p, fz_invert_matrix (pagectm (page)));

    for (link = page->links; link; link = link->next) {
        if (p.x >= link->rect.x0 && p.x <= link->rect.x1) {
            if (p.y >= link->rect.y0 && p.y <= link->rect.y1) {
                return link;
            }
        }
    }
    return NULL;
}

static void ensuretext (struct page *page)
{
    if (state.gen != page->tgen) {
        droptext (page);
        page->tgen = state.gen;
    }
    if (!page->text) {
        fz_device *tdev;

        page->text = fz_new_stext_page (state.ctx,
                                        state.pagedims[page->pdimno].mediabox);
        tdev = fz_new_stext_device (state.ctx, page->text, 0);
        fz_run_display_list (state.ctx, page->dlist,
                             tdev, pagectm (page), fz_infinite_rect, NULL);
        fz_close_device (state.ctx, tdev);
        fz_drop_device (state.ctx, tdev);
    }
}

value ml_find_page_with_links (value start_page_v, value dir_v);
value ml_find_page_with_links (value start_page_v, value dir_v)
{
    CAMLparam2 (start_page_v, dir_v);
    CAMLlocal1 (ret_v);
    int i, dir = Int_val (dir_v);
    int start_page = Int_val (start_page_v);
    int end_page = dir > 0 ? state.pagecount : -1;
    pdf_document *pdf;

    fz_var (end_page);
    ret_v = Val_int (0);
    lock (__func__);
    pdf = pdf_specifics (state.ctx, state.doc);
    for (i = start_page + dir; i != end_page; i += dir) {
        int found;

        fz_var (found);
        if (pdf) {
            pdf_page *page = NULL;

            fz_var (page);
            fz_try (state.ctx) {
                page = pdf_load_page (state.ctx, pdf, i);
                found = !!page->links || !!page->annots;
            }
            fz_catch (state.ctx) {
                found = 0;
            }
            if (page) {
                fz_drop_page (state.ctx, &page->super);
            }
        }
        else {
            fz_page *page = fz_load_page (state.ctx, state.doc, i);
            fz_link *link = fz_load_links (state.ctx, page);
            found = !!link;
            fz_drop_link (state.ctx, link);
            fz_drop_page (state.ctx, page);
        }

        if (found) {
            ret_v = caml_alloc_small (1, 1);
            Field (ret_v, 0) = Val_int (i);
            goto unlock;
        }
    }
 unlock:
    unlock (__func__);
    CAMLreturn (ret_v);
}

enum { dir_first, dir_last };
enum { dir_first_visible, dir_left, dir_right, dir_down, dir_up };

value ml_findlink (value ptr_v, value dir_v);
value ml_findlink (value ptr_v, value dir_v)
{
    CAMLparam2 (ptr_v, dir_v);
    CAMLlocal2 (ret_v, pos_v);
    struct page *page;
    int dirtag, i, slinkindex;
    struct slink *found = NULL ,*slink;
    char *s = String_val (ptr_v);

    page = parse_pointer (__func__, s);
    ret_v = Val_int (0);
    lock (__func__);
    ensureslinks (page);

    if (Is_block (dir_v)) {
        dirtag = Tag_val (dir_v);
        switch (dirtag) {
        case dir_first_visible:
            {
                int x0, y0, dir, first_index, last_index;

                pos_v = Field (dir_v, 0);
                x0 = Int_val (Field (pos_v, 0));
                y0 = Int_val (Field (pos_v, 1));
                dir = Int_val (Field (pos_v, 2));

                if (dir >= 0) {
                    dir = 1;
                    first_index = 0;
                    last_index = page->slinkcount;
                }
                else {
                    first_index = page->slinkcount - 1;
                    last_index = -1;
                }

                for (i = first_index; i != last_index; i += dir) {
                    slink = &page->slinks[i];
                    if (slink->bbox.y0 >= y0 && slink->bbox.x0 >= x0) {
                        found = slink;
                        break;
                    }
                }
            }
            break;

        case dir_left:
            slinkindex = Int_val (Field (dir_v, 0));
            found = &page->slinks[slinkindex];
            for (i = slinkindex - 1; i >= 0; --i) {
                slink = &page->slinks[i];
                if (slink->bbox.x0 < found->bbox.x0) {
                    found = slink;
                    break;
                }
            }
            break;

        case dir_right:
            slinkindex = Int_val (Field (dir_v, 0));
            found = &page->slinks[slinkindex];
            for (i = slinkindex + 1; i < page->slinkcount; ++i) {
                slink = &page->slinks[i];
                if (slink->bbox.x0 > found->bbox.x0) {
                    found = slink;
                    break;
                }
            }
            break;

        case dir_down:
            slinkindex = Int_val (Field (dir_v, 0));
            found = &page->slinks[slinkindex];
            for (i = slinkindex + 1; i < page->slinkcount; ++i) {
                slink = &page->slinks[i];
                if (slink->bbox.y0 >= found->bbox.y0) {
                    found = slink;
                    break;
                }
            }
            break;

        case dir_up:
            slinkindex = Int_val (Field (dir_v, 0));
            found = &page->slinks[slinkindex];
            for (i = slinkindex - 1; i >= 0; --i) {
                slink = &page->slinks[i];
                if (slink->bbox.y0 <= found->bbox.y0) {
                    found = slink;
                    break;
                }
            }
            break;
        }
    }
    else {
        dirtag = Int_val (dir_v);
        switch (dirtag) {
        case dir_first:
            found = page->slinks;
            break;

        case dir_last:
            if (page->slinks) {
                found = page->slinks + (page->slinkcount - 1);
            }
            break;
        }
    }
    if (found) {
        ret_v = caml_alloc_small (2, 1);
        Field (ret_v, 0) = Val_int (found - page->slinks);
    }

    unlock (__func__);
    CAMLreturn (ret_v);
}

enum { uuri, utext, uannot };

value ml_getlink (value ptr_v, value n_v);
value ml_getlink (value ptr_v, value n_v)
{
    CAMLparam2 (ptr_v, n_v);
    CAMLlocal4 (ret_v, tup_v, str_v, gr_v);
    fz_link *link;
    struct page *page;
    char *s = String_val (ptr_v);
    struct slink *slink;

    ret_v = Val_int (0);
    page = parse_pointer (__func__, s);

    lock (__func__);
    ensureslinks (page);
    slink = &page->slinks[Int_val (n_v)];
    if (slink->tag == SLINK) {
        link = slink->u.link;
        str_v = caml_copy_string (link->uri);
        ret_v = caml_alloc_small (1, uuri);
        Field (ret_v, 0) = str_v;
    }
    else {
        ret_v = caml_alloc_small (1, uannot);
        tup_v = caml_alloc_tuple (2);
        Field (ret_v, 0) = tup_v;
        Field (tup_v, 0) = ptr_v;
        Field (tup_v, 1) = n_v;
    }
    unlock (__func__);

    CAMLreturn (ret_v);
}

value ml_getannotcontents (value ptr_v, value n_v);
value ml_getannotcontents (value ptr_v, value n_v)
{
    CAMLparam2 (ptr_v, n_v);
    CAMLlocal1 (ret_v);
    pdf_document *pdf;
    const char *contents = "";

    lock (__func__);
    pdf = pdf_specifics (state.ctx, state.doc);
    if (pdf) {
        char *s = String_val (ptr_v);
        struct page *page;
        struct slink *slink;

        page = parse_pointer (__func__, s);
        slink = &page->slinks[Int_val (n_v)];
        contents = pdf_annot_contents (state.ctx, (pdf_annot *) slink->u.annot);
    }
    unlock (__func__);
    ret_v = caml_copy_string (contents);
    CAMLreturn (ret_v);
}

value ml_getlinkcount (value ptr_v);
value ml_getlinkcount (value ptr_v)
{
    CAMLparam1 (ptr_v);
    struct page *page;
    char *s = String_val (ptr_v);

    page = parse_pointer (__func__, s);
    CAMLreturn (Val_int (page->slinkcount));
}

value ml_getlinkrect (value ptr_v, value n_v);
value ml_getlinkrect (value ptr_v, value n_v)
{
    CAMLparam2 (ptr_v, n_v);
    CAMLlocal1 (ret_v);
    struct page *page;
    struct slink *slink;
    char *s = String_val (ptr_v);

    page = parse_pointer (__func__, s);
    ret_v = caml_alloc_tuple (4);
    lock (__func__);
    ensureslinks (page);

    slink = &page->slinks[Int_val (n_v)];
    Field (ret_v, 0) = Val_int (slink->bbox.x0);
    Field (ret_v, 1) = Val_int (slink->bbox.y0);
    Field (ret_v, 2) = Val_int (slink->bbox.x1);
    Field (ret_v, 3) = Val_int (slink->bbox.y1);
    unlock (__func__);
    CAMLreturn (ret_v);
}

value ml_whatsunder (value ptr_v, value x_v, value y_v);
value ml_whatsunder (value ptr_v, value x_v, value y_v)
{
    CAMLparam3 (ptr_v, x_v, y_v);
    CAMLlocal4 (ret_v, tup_v, str_v, gr_v);
    fz_link *link;
    struct annot *annot;
    struct page *page;
    char *ptr = String_val (ptr_v);
    int x = Int_val (x_v), y = Int_val (y_v);
    struct pagedim *pdim;

    ret_v = Val_int (0);
    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, ptr);
    pdim = &state.pagedims[page->pdimno];
    x += pdim->bounds.x0;
    y += pdim->bounds.y0;

    {
        annot = getannot (page, x, y);
        if (annot) {
            int i, n = -1;

            ensureslinks (page);
            for (i = 0; i < page->slinkcount; ++i) {
                if (page->slinks[i].tag == SANNOT
                    && page->slinks[i].u.annot == annot->annot) {
                    n = i;
                    break;
                }
            }
            ret_v = caml_alloc_small (1, uannot);
            tup_v = caml_alloc_tuple (2);
            Field (ret_v, 0) = tup_v;
            Field (tup_v, 0) = ptr_v;
            Field (tup_v, 1) = Val_int (n);
            goto unlock;
        }
    }

    link = getlink (page, x, y);
    if (link) {
        str_v = caml_copy_string (link->uri);
        ret_v = caml_alloc_small (1, uuri);
        Field (ret_v, 0) = str_v;
    }
    else {
        fz_rect *b;
        fz_stext_block *block;

        ensuretext (page);

        for (block = page->text->first_block; block; block = block->next) {
            fz_stext_line *line;

            if (block->type != FZ_STEXT_BLOCK_TEXT) continue;
            b = &block->bbox;
            if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                continue;

            for (line = block->u.t.first_line; line; line = line->next) {
                fz_stext_char *ch;

                b = &line->bbox;
                if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                    continue;

                for (ch = line->first_char; ch; ch = ch->next) {
                    fz_quad *q = &ch->quad;

                    if (x >= q->ul.x && x <= q->ur.x
                        && y >= q->ul.y && y <= q->ll.y) {
                        const char *n2 = fz_font_name (state.ctx, ch->font);
                        FT_FaceRec *face = fz_font_ft_face (state.ctx,
                                                            ch->font);

                        if (!n2) n2 = "<unknown font>";

                        if (face && face->family_name) {
                            char *s;
                            char *n1 = face->family_name;
                            size_t l1 = strlen (n1);
                            size_t l2 = strlen (n2);

                            if (l1 != l2 || memcmp (n1, n2, l1)) {
                                s = malloc (l1 + l2 + 2);
                                if (s) {
                                    memcpy (s, n2, l2);
                                    s[l2] = '=';
                                    memcpy (s + l2 + 1, n1, l1 + 1);
                                    str_v = caml_copy_string (s);
                                    free (s);
                                }
                            }
                        }
                        if (str_v == Val_unit) {
                            str_v = caml_copy_string (n2);
                        }
                        ret_v = caml_alloc_small (1, utext);
                        Field (ret_v, 0) = str_v;
                        goto unlock;
                    }
                }
            }
        }
    }
unlock:
    unlock (__func__);

done:
    CAMLreturn (ret_v);
}

enum { mark_page, mark_block, mark_line, mark_word };

void ml_clearmark (value ptr_v);
void ml_clearmark (value ptr_v)
{
    CAMLparam1 (ptr_v);
    char *s = String_val (ptr_v);
    struct page *page;

    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);
    page->fmark = NULL;
    page->lmark = NULL;

    unlock (__func__);
 done:
    CAMLreturn0;
}

static int uninteresting (int c)
{
    return isspace (c) || ispunct (c);
}

value ml_markunder (value ptr_v, value x_v, value y_v, value mark_v);
value ml_markunder (value ptr_v, value x_v, value y_v, value mark_v)
{
    CAMLparam4 (ptr_v, x_v, y_v, mark_v);
    CAMLlocal1 (ret_v);
    fz_rect *b;
    struct page *page;
    fz_stext_line *line;
    fz_stext_block *block;
    struct pagedim *pdim;
    int mark = Int_val (mark_v);
    char *s = String_val (ptr_v);
    int x = Int_val (x_v), y = Int_val (y_v);

    ret_v = Val_bool (0);
    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);
    pdim = &state.pagedims[page->pdimno];

    ensuretext (page);

    if (mark == mark_page) {
        page->fmark = page->text->first_block->u.t.first_line->first_char;
        page->lmark = page->text->last_block->u.t.last_line->last_char;
        ret_v = Val_bool (1);
        goto unlock;
    }

    x += pdim->bounds.x0;
    y += pdim->bounds.y0;

    for (block = page->text->first_block; block; block = block->next) {
        if (block->type != FZ_STEXT_BLOCK_TEXT) continue;
        b = &block->bbox;
        if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
            continue;

        if (mark == mark_block) {
            page->fmark = block->u.t.first_line->first_char;
            page->lmark = block->u.t.last_line->last_char;
            ret_v = Val_bool (1);
            goto unlock;
        }

        for (line = block->u.t.first_line; line; line = line->next) {
            fz_stext_char *ch;

            b = &line->bbox;
            if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                continue;

            if (mark == mark_line) {
                page->fmark = line->first_char;
                page->lmark = line->last_char;
                ret_v = Val_bool (1);
                goto unlock;
            }

            for (ch = line->first_char; ch; ch = ch->next) {
                fz_stext_char *ch2, *first = NULL, *last = NULL;
                fz_quad *q = &ch->quad;
                if (x >= q->ul.x && x <= q->ur.x
                    && y >= q->ul.y && y <= q->ll.y) {
                    for (ch2 = line->first_char; ch2 != ch; ch2 = ch2->next) {
                        if (uninteresting (ch2->c)) first = NULL;
                        else if (!first) first = ch2;
                    }
                    for (ch2 = ch; ch2; ch2 = ch2->next) {
                        if (uninteresting (ch2->c)) break;
                        last = ch2;
                    }

                    page->fmark = first;
                    page->lmark = last;
                    ret_v = Val_bool (1);
                    goto unlock;
                }
            }
        }
    }
unlock:
    if (!Bool_val (ret_v)) {
        page->fmark = NULL;
        page->lmark = NULL;
    }
    unlock (__func__);

done:
    CAMLreturn (ret_v);
}

value ml_rectofblock (value ptr_v, value x_v, value y_v);
value ml_rectofblock (value ptr_v, value x_v, value y_v)
{
    CAMLparam3 (ptr_v, x_v, y_v);
    CAMLlocal2 (ret_v, res_v);
    fz_rect *b = NULL;
    struct page *page;
    struct pagedim *pdim;
    fz_stext_block *block;
    char *s = String_val (ptr_v);
    int x = Int_val (x_v), y = Int_val (y_v);

    ret_v = Val_int (0);
    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);
    pdim = &state.pagedims[page->pdimno];
    x += pdim->bounds.x0;
    y += pdim->bounds.y0;

    ensuretext (page);

    for (block = page->text->first_block; block; block = block->next) {
        switch (block->type) {
        case FZ_STEXT_BLOCK_TEXT:
            b = &block->bbox;
            break;

        case FZ_STEXT_BLOCK_IMAGE:
            b = &block->bbox;
            break;

        default:
            continue;
        }

        if (x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1)
            break;
        b = NULL;
    }
    if (b) {
        res_v = caml_alloc_small (4 * Double_wosize, Double_array_tag);
        ret_v = caml_alloc_small (1, 1);
        Store_double_field (res_v, 0, (double) b->x0);
        Store_double_field (res_v, 1, (double) b->x1);
        Store_double_field (res_v, 2, (double) b->y0);
        Store_double_field (res_v, 3, (double) b->y1);
        Field (ret_v, 0) = res_v;
    }
    unlock (__func__);

 done:
    CAMLreturn (ret_v);
}

void ml_seltext (value ptr_v, value rect_v);
void ml_seltext (value ptr_v, value rect_v)
{
    CAMLparam2 (ptr_v, rect_v);
    struct page *page;
    struct pagedim *pdim;
    char *s = String_val (ptr_v);
    int x0, x1, y0, y1;
    fz_stext_char *ch;
    fz_stext_line *line;
    fz_stext_block *block;
    fz_stext_char *fc, *lc;

    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);
    ensuretext (page);

    pdim = &state.pagedims[page->pdimno];
    x0 = Int_val (Field (rect_v, 0)) + pdim->bounds.x0;
    y0 = Int_val (Field (rect_v, 1)) + pdim->bounds.y0;
    x1 = Int_val (Field (rect_v, 2)) + pdim->bounds.x0;
    y1 = Int_val (Field (rect_v, 3)) + pdim->bounds.y0;

    if (y0 > y1) {
        int t = y0;
        y0 = y1;
        y1 = t;
        x0 = x1;
        x1 = t;
    }

    fc = page->fmark;
    lc = page->lmark;

    for (block = page->text->first_block; block; block = block->next) {
        if (block->type != FZ_STEXT_BLOCK_TEXT) continue;
        for (line = block->u.t.first_line; line; line = line->next) {
            for (ch = line->first_char; ch; ch = ch->next) {
                fz_quad q = ch->quad;
                if (x0 >= q.ul.x && x0 <= q.ur.x
                    && y0 >= q.ul.y && y0 <= q.ll.y) {
                    fc = ch;
                }
                if (x1 >= q.ul.x && x1 <= q.ur.x
                    && y1 >= q.ul.y && y1 <= q.ll.y) {
                    lc = ch;
                }
            }
        }
    }
    if (x1 < x0 && fc == lc) {
        fz_stext_char *t;

        t = fc;
        fc = lc;
        lc = t;
    }

    page->fmark = fc;
    page->lmark = lc;

    unlock (__func__);

 done:
    CAMLreturn0;
}

static int pipechar (FILE *f, fz_stext_char *ch)
{
    char buf[4];
    int len;
    size_t ret;

    len = fz_runetochar (buf, ch->c);
    ret = fwrite (buf, len, 1, f);
    if (ret != 1) {
        printd ("emsg failed to write %d bytes ret=%zu: %d:%s",
                len, ret, errno, strerror (errno));
        return -1;
    }
    return 0;
}

value ml_spawn (value command_v, value fds_v);
value ml_spawn (value command_v, value fds_v)
{
    CAMLparam2 (command_v, fds_v);
    CAMLlocal2 (l_v, tup_v);
    int ret, ret1;
    pid_t pid = (pid_t) -1;
    char *msg = NULL;
    value earg_v = Nothing;
    posix_spawnattr_t attr;
    posix_spawn_file_actions_t fa;
    char *argv[] = { "/bin/sh", "-c", NULL, NULL };

    argv[2] = String_val (command_v);

    if ((ret = posix_spawn_file_actions_init (&fa)) != 0) {
        unix_error (ret, "posix_spawn_file_actions_init", Nothing);
    }

    if ((ret = posix_spawnattr_init (&attr)) != 0) {
        msg = "posix_spawnattr_init";
        goto fail1;
    }

#ifdef POSIX_SPAWN_USEVFORK
    if ((ret = posix_spawnattr_setflags (&attr, POSIX_SPAWN_USEVFORK)) != 0) {
        msg =  "posix_spawnattr_setflags POSIX_SPAWN_USEVFORK";
        goto fail;
    }
#endif

    for (l_v = fds_v; l_v != Val_int (0); l_v = Field (l_v, 1)) {
        int fd1, fd2;

        tup_v = Field (l_v, 0);
        fd1 = Int_val (Field (tup_v, 0));
        fd2 = Int_val (Field (tup_v, 1));
        if (fd2 < 0) {
            if ((ret = posix_spawn_file_actions_addclose (&fa, fd1)) != 0) {
                msg = "posix_spawn_file_actions_addclose";
                earg_v = tup_v;
                goto fail;
            }
        }
        else {
            if ((ret = posix_spawn_file_actions_adddup2 (&fa, fd1, fd2)) != 0) {
                msg = "posix_spawn_file_actions_adddup2";
                earg_v = tup_v;
                goto fail;
            }
        }
    }

    if ((ret = posix_spawn (&pid, "/bin/sh", &fa, &attr, argv, environ))) {
        msg = "posix_spawn";
        goto fail;
    }

 fail:
    if ((ret1 = posix_spawnattr_destroy (&attr)) != 0) {
        printd ("emsg posix_spawnattr_destroy: %d:%s", ret1, strerror (ret1));
    }

 fail1:
    if ((ret1 = posix_spawn_file_actions_destroy (&fa)) != 0) {
        printd ("emsg posix_spawn_file_actions_destroy: %d:%s",
                ret1, strerror (ret1));
    }

    if (msg)
        unix_error (ret, msg, earg_v);

    CAMLreturn (Val_int (pid));
}

value ml_hassel (value ptr_v);
value ml_hassel (value ptr_v)
{
    CAMLparam1 (ptr_v);
    CAMLlocal1 (ret_v);
    struct page *page;
    char *s = String_val (ptr_v);

    ret_v = Val_bool (0);
    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);
    ret_v = Val_bool (page->fmark && page->lmark);
    unlock (__func__);
 done:
    CAMLreturn (ret_v);
}

void ml_copysel (value fd_v, value ptr_v);
void ml_copysel (value fd_v, value ptr_v)
{
    CAMLparam2 (fd_v, ptr_v);
    FILE *f;
    int seen = 0;
    struct page *page;
    fz_stext_line *line;
    fz_stext_block *block;
    int fd = Int_val (fd_v);
    char *s = String_val (ptr_v);

    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);

    if (!page->fmark || !page->lmark) {
        printd ("emsg nothing to copy on page %d", page->pageno);
        goto unlock;
    }

    f = fdopen (fd, "w");
    if (!f) {
        printd ("emsg failed to fdopen sel pipe (from fd %d): %d:%s",
                fd, errno, strerror (errno));
        f = stdout;
    }

    for (block = page->text->first_block; block; block = block->next) {
        if (block->type != FZ_STEXT_BLOCK_TEXT) continue;
        for (line = block->u.t.first_line; line; line = line->next) {
            fz_stext_char *ch;
            for (ch = line->first_char; ch; ch = ch->next) {
                if (seen || ch == page->fmark) {
                    do {
                        pipechar (f, ch);
                        if (ch == page->lmark) goto close;
                    } while ((ch = ch->next));
                    seen = 1;
                    break;
                }
            }
            if (seen) fputc ('\n', f);
        }
    }
close:
    if (f != stdout) {
        int ret = fclose (f);
        fd = -1;
        if (ret == -1) {
            if (errno != ECHILD) {
                printd ("emsg failed to close sel pipe: %d:%s",
                        errno, strerror (errno));
            }
        }
    }
unlock:
    unlock (__func__);

done:
    if (fd >= 0) {
        if (close (fd)) {
            printd ("emsg failed to close sel pipe: %d:%s",
                    errno, strerror (errno));
        }
    }
    CAMLreturn0;
}

value ml_getpdimrect (value pagedimno_v);
value ml_getpdimrect (value pagedimno_v)
{
    CAMLparam1 (pagedimno_v);
    CAMLlocal1 (ret_v);
    int pagedimno = Int_val (pagedimno_v);
    fz_rect box;

    ret_v = caml_alloc_small (4 * Double_wosize, Double_array_tag);
    if (trylock (__func__)) {
        box = fz_empty_rect;
    }
    else {
        box = state.pagedims[pagedimno].mediabox;
        unlock (__func__);
    }

    Store_double_field (ret_v, 0, (double) box.x0);
    Store_double_field (ret_v, 1, (double) box.x1);
    Store_double_field (ret_v, 2, (double) box.y0);
    Store_double_field (ret_v, 3, (double) box.y1);

    CAMLreturn (ret_v);
}

value ml_zoom_for_height (value winw_v, value winh_v,
                          value dw_v, value cols_v);
value ml_zoom_for_height (value winw_v, value winh_v,
                          value dw_v, value cols_v)
{
    CAMLparam4 (winw_v, winh_v, dw_v, cols_v);
    CAMLlocal1 (ret_v);
    int i;
    float zoom = -1.;
    float maxh = 0.0;
    struct pagedim *p;
    float winw = Int_val (winw_v);
    float winh = Int_val (winh_v);
    float dw = Int_val (dw_v);
    float cols = Int_val (cols_v);
    float pw = 1.0, ph = 1.0;

    if (trylock (__func__)) {
        goto done;
    }

    for (i = 0, p = state.pagedims; i < state.pagedimcount; ++i, ++p) {
        float w = p->pagebox.x1 / cols;
        float h = p->pagebox.y1;
        if (h > maxh) {
            maxh = h;
            ph = h;
            if (state.fitmodel != FitProportional) pw = w;
        }
        if ((state.fitmodel == FitProportional) && w > pw) pw = w;
    }

    zoom = (((winh / ph) * pw) + dw) / winw;
    unlock (__func__);
 done:
    ret_v = caml_copy_double ((double) zoom);
    CAMLreturn (ret_v);
}

value ml_getmaxw (value unit_v);
value ml_getmaxw (value unit_v)
{
    CAMLparam1 (unit_v);
    CAMLlocal1 (ret_v);
    int i;
    float maxw = -1.;
    struct pagedim *p;

    if (trylock (__func__)) {
        goto done;
    }

    for (i = 0, p = state.pagedims; i < state.pagedimcount; ++i, ++p) {
        float w = p->pagebox.x1;
        maxw = fz_max (maxw, w);
    }

    unlock (__func__);
 done:
    ret_v = caml_copy_double ((double) maxw);
    CAMLreturn (ret_v);
}

value ml_draw_string (value pt_v, value x_v,
                      value y_v, value string_v);
value ml_draw_string (value pt_v, value x_v,
                      value y_v, value string_v)
{
    CAMLparam4 (pt_v, x_v, y_v, string_v);
    CAMLlocal1 (ret_v);
    int pt = Int_val(pt_v);
    int x = Int_val (x_v);
    int y = Int_val (y_v);
    float w;

    w = draw_string (state.face, pt, x, y, String_val (string_v));
    ret_v = caml_copy_double (w);
    CAMLreturn (ret_v);
}

value ml_measure_string (value pt_v, value string_v);
value ml_measure_string (value pt_v, value string_v)
{
    CAMLparam2 (pt_v, string_v);
    CAMLlocal1 (ret_v);
    int pt = Int_val (pt_v);
    double w;

    w = (double) measure_string (state.face, pt, String_val (string_v));
    ret_v = caml_copy_double (w);
    CAMLreturn (ret_v);
}

value ml_getpagebox (value opaque_v);
value ml_getpagebox (value opaque_v)
{
    CAMLparam1 (opaque_v);
    CAMLlocal1 (ret_v);
    fz_rect rect;
    fz_irect bbox;
    fz_device *dev;
    char *s = String_val (opaque_v);
    struct page *page = parse_pointer (__func__, s);

    ret_v = caml_alloc_tuple (4);
    dev = fz_new_bbox_device (state.ctx, &rect);

    fz_run_page (state.ctx, page->fzpage, dev, pagectm (page), NULL);

    fz_close_device (state.ctx, dev);
    fz_drop_device (state.ctx, dev);
    bbox = fz_round_rect (rect);
    Field (ret_v, 0) = Val_int (bbox.x0);
    Field (ret_v, 1) = Val_int (bbox.y0);
    Field (ret_v, 2) = Val_int (bbox.x1);
    Field (ret_v, 3) = Val_int (bbox.y1);

    CAMLreturn (ret_v);
}

void ml_setaalevel (value level_v);
void ml_setaalevel (value level_v)
{
    CAMLparam1 (level_v);

    state.aalevel = Int_val (level_v);
    CAMLreturn0;
}

value ml_keysymtoutf8 (value keysym_v);
#ifndef CIDER
value ml_keysymtoutf8 (value keysym_v)
{
    CAMLparam1 (keysym_v);
    CAMLlocal1 (str_v);
    KeySym keysym = Int_val (keysym_v);
    Rune rune;
    extern long keysym2ucs (KeySym);
    int len;
    char buf[5];

    rune = (Rune) keysym2ucs (keysym);
    len = fz_runetochar (buf, rune);
    buf[len] = 0;
    str_v = caml_copy_string (buf);
    CAMLreturn (str_v);
}
#else
value ml_keysymtoutf8 (value keysym_v)
{
    CAMLparam1 (keysym_v);
    CAMLlocal1 (str_v);
    long ucs = Long_val (keysym_v);
    int len;
    char buf[5];

    len = fz_runetochar (buf, (int) ucs);
    buf[len] = 0;
    str_v = caml_copy_string (buf);
    CAMLreturn (str_v);
}
#endif

enum { piunknown, pilinux, pimacos, pibsd };

value ml_platform (value unit_v);
value ml_platform (value unit_v)
{
    CAMLparam1 (unit_v);
    CAMLlocal2 (tup_v, arr_v);
    int platid = piunknown;
    struct utsname buf;

#if defined __linux__
    platid = pilinux;
#elif defined __DragonFly__ || defined __FreeBSD__
    || defined __OpenBSD__ || defined __NetBSD__
    platid = pibsd;
#elif defined __APPLE__
    platid = pimacos;
#endif
    if (uname (&buf)) err (1, "uname");

    tup_v = caml_alloc_tuple (2);
    {
        char const *sar[] = {
            buf.sysname,
            buf.release,
            buf.version,
            buf.machine,
            NULL
        };
        arr_v = caml_copy_string_array (sar);
    }
    Field (tup_v, 0) = Val_int (platid);
    Field (tup_v, 1) = arr_v;
    CAMLreturn (tup_v);
}

void ml_cloexec (value fd_v);
void ml_cloexec (value fd_v)
{
    CAMLparam1 (fd_v);
    int fd = Int_val (fd_v);

    if (fcntl (fd, F_SETFD, FD_CLOEXEC, 1)) {
        uerror ("fcntl", Nothing);
    }
    CAMLreturn0;
}

value ml_getpbo (value w_v, value h_v, value cs_v);
value ml_getpbo (value w_v, value h_v, value cs_v)
{
    CAMLparam2 (w_v, h_v);
    CAMLlocal1 (ret_v);
    struct bo *pbo;
    int w = Int_val (w_v);
    int h = Int_val (h_v);
    int cs = Int_val (cs_v);

    if (state.bo_usable) {
        pbo = calloc (sizeof (*pbo), 1);
        if (!pbo) {
            err (1, "calloc pbo");
        }

        switch (cs) {
        case 0:
        case 1:
            pbo->size = w*h*4;
            break;
        case 2:
            pbo->size = w*h*2;
            break;
        default:
            errx (1, "%s: invalid colorspace %d", __func__, cs);
        }

        state.glGenBuffersARB (1, &pbo->id);
        state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, pbo->id);
        state.glBufferDataARB (GL_PIXEL_UNPACK_BUFFER_ARB, (GLsizei) pbo->size,
                               NULL, GL_STREAM_DRAW);
        pbo->ptr = state.glMapBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB,
                                         GL_READ_WRITE);
        state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, 0);
        if (!pbo->ptr) {
            printd ("emsg glMapBufferARB failed: %#x", glGetError ());
            state.glDeleteBuffersARB (1, &pbo->id);
            free (pbo);
            ret_v = caml_copy_string ("0");
        }
        else {
            int res;
            char *s;

            res = snprintf (NULL, 0, "%" PRIxPTR, (uintptr_t) pbo);
            if (res < 0) {
                err (1, "snprintf %" PRIxPTR " failed", (uintptr_t) pbo);
            }
            s = malloc (res+1);
            if (!s) {
                err (1, "malloc %d bytes failed", res+1);
            }
            res = sprintf (s, "%" PRIxPTR, (uintptr_t) pbo);
            if (res < 0) {
                err (1, "sprintf %" PRIxPTR " failed", (uintptr_t) pbo);
            }
            ret_v = caml_copy_string (s);
            free (s);
        }
    }
    else {
        ret_v = caml_copy_string ("0");
    }
    CAMLreturn (ret_v);
}

void ml_freepbo (value s_v);
void ml_freepbo (value s_v)
{
    CAMLparam1 (s_v);
    char *s = String_val (s_v);
    struct tile *tile = parse_pointer (__func__, s);

    if (tile->pbo) {
        state.glDeleteBuffersARB (1, &tile->pbo->id);
        tile->pbo->id = -1;
        tile->pbo->ptr = NULL;
        tile->pbo->size = -1;
    }
    CAMLreturn0;
}

void ml_unmappbo (value s_v);
void ml_unmappbo (value s_v)
{
    CAMLparam1 (s_v);
    char *s = String_val (s_v);
    struct tile *tile = parse_pointer (__func__, s);

    if (tile->pbo) {
        state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, tile->pbo->id);
        if (state.glUnmapBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB) == GL_FALSE) {
            errx (1, "glUnmapBufferARB failed: %#x\n", glGetError ());
        }
        tile->pbo->ptr = NULL;
        state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, 0);
    }
    CAMLreturn0;
}

static void setuppbo (void)
{
    extern void (*wsigladdr (const char *name)) (void);
#pragma GCC diagnostic push
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wbad-function-cast"
#endif
#define GPA(n) (*(uintptr_t *) &state.n = (uintptr_t) wsigladdr (#n))
    state.bo_usable = GPA (glBindBufferARB)
        && GPA (glUnmapBufferARB)
        && GPA (glMapBufferARB)
        && GPA (glBufferDataARB)
        && GPA (glGenBuffersARB)
        && GPA (glDeleteBuffersARB);
#undef GPA
#pragma GCC diagnostic pop
}

value ml_bo_usable (void);
value ml_bo_usable (void)
{
    return Val_bool (state.bo_usable);
}

value ml_unproject (value ptr_v, value x_v, value y_v);
value ml_unproject (value ptr_v, value x_v, value y_v)
{
    CAMLparam3 (ptr_v, x_v, y_v);
    CAMLlocal2 (ret_v, tup_v);
    struct page *page;
    char *s = String_val (ptr_v);
    int x = Int_val (x_v), y = Int_val (y_v);
    struct pagedim *pdim;
    fz_point p;

    page = parse_pointer (__func__, s);
    pdim = &state.pagedims[page->pdimno];

    ret_v = Val_int (0);
    if (trylock (__func__)) {
        goto done;
    }

    p.x = x + pdim->bounds.x0;
    p.y = y + pdim->bounds.y0;

    p = fz_transform_point (p, fz_invert_matrix (fz_concat (pdim->tctm,
                                                            pdim->ctm)));

    tup_v = caml_alloc_tuple (2);
    ret_v = caml_alloc_small (1, 1);
    Field (tup_v, 0) = Val_int (p.x);
    Field (tup_v, 1) = Val_int (p.y);
    Field (ret_v, 0) = tup_v;

    unlock (__func__);
 done:
    CAMLreturn (ret_v);
}

value ml_project (value ptr_v, value pageno_v, value pdimno_v,
                  value x_v, value y_v);
value ml_project (value ptr_v, value pageno_v, value pdimno_v,
                  value x_v, value y_v)
{
    CAMLparam5 (ptr_v, pageno_v, pdimno_v, x_v, y_v);
    CAMLlocal1 (ret_v);
    struct page *page;
    char *s = String_val (ptr_v);
    int pageno = Int_val (pageno_v);
    int pdimno = Int_val (pdimno_v);
    float x = (float) Double_val (x_v), y = (float) Double_val (y_v);
    struct pagedim *pdim;
    fz_point p;
    fz_matrix ctm;

    ret_v = Val_int (0);
    lock (__func__);

    if (!*s) {
        page = loadpage (pageno, pdimno);
    }
    else {
        page = parse_pointer (__func__, s);
    }
    pdim = &state.pagedims[pdimno];

    if (pdf_specifics (state.ctx, state.doc)) {
        trimctm (pdf_page_from_fz_page (state.ctx, page->fzpage), page->pdimno);
        ctm = state.pagedims[page->pdimno].tctm;
    }
    else {
        ctm = fz_identity;
    }
    p.x = x + pdim->bounds.x0;
    p.y = y + pdim->bounds.y0;

    ctm = fz_concat (pdim->tctm, pdim->ctm);
    p = fz_transform_point (p, ctm);

    ret_v = caml_alloc_tuple (2);
    Field (ret_v, 0) = caml_copy_double ((double) p.x);
    Field (ret_v, 1) = caml_copy_double ((double) p.y);

    if (!*s) {
        freepage (page);
    }
    unlock (__func__);
    CAMLreturn (ret_v);
}

void ml_addannot (value ptr_v, value x_v, value y_v, value contents_v);
void ml_addannot (value ptr_v, value x_v, value y_v, value contents_v)
{
    CAMLparam4 (ptr_v, x_v, y_v, contents_v);
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    if (pdf) {
        pdf_annot *annot;
        struct page *page;
        fz_rect r;
        char *s = String_val (ptr_v);

        page = parse_pointer (__func__, s);
        annot = pdf_create_annot (state.ctx,
                                  pdf_page_from_fz_page (state.ctx,
                                                         page->fzpage),
                                  PDF_ANNOT_TEXT);
        r.x0 = Int_val (x_v) - 10;
        r.y0 = Int_val (y_v) - 10;
        r.x1 = r.x0 + 20;
        r.y1 = r.y0 + 20;
        pdf_set_annot_contents (state.ctx, annot, String_val (contents_v));
        pdf_set_annot_rect (state.ctx, annot, r);

        state.dirty = 1;
    }
    CAMLreturn0;
}

void ml_delannot (value ptr_v, value n_v);
void ml_delannot (value ptr_v, value n_v)
{
    CAMLparam2 (ptr_v, n_v);
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    if (pdf) {
        struct page *page;
        char *s = String_val (ptr_v);
        struct slink *slink;

        page = parse_pointer (__func__, s);
        slink = &page->slinks[Int_val (n_v)];
        pdf_delete_annot (state.ctx,
                          pdf_page_from_fz_page (state.ctx, page->fzpage),
                          (pdf_annot *) slink->u.annot);
        state.dirty = 1;
    }
    CAMLreturn0;
}

void ml_modannot (value ptr_v, value n_v, value str_v);
void ml_modannot (value ptr_v, value n_v, value str_v)
{
    CAMLparam3 (ptr_v, n_v, str_v);
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    if (pdf) {
        struct page *page;
        char *s = String_val (ptr_v);
        struct slink *slink;

        page = parse_pointer (__func__, s);
        slink = &page->slinks[Int_val (n_v)];
        pdf_set_annot_contents (state.ctx, (pdf_annot *) slink->u.annot,
                                String_val (str_v));
        state.dirty = 1;
    }
    CAMLreturn0;
}

value ml_hasunsavedchanges (void);
value ml_hasunsavedchanges (void)
{
    return Val_bool (state.dirty);
}

void ml_savedoc (value path_v);
void ml_savedoc (value path_v)
{
    CAMLparam1 (path_v);
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    if (pdf) {
        pdf_save_document (state.ctx, pdf, String_val (path_v), NULL);
    }
    CAMLreturn0;
}

static void makestippletex (void)
{
    const char pixels[] = "\xff\xff\0\0";
    glGenTextures (1, &state.stid);
    glBindTexture (GL_TEXTURE_1D, state.stid);
    glTexParameteri (GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri (GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage1D (
        GL_TEXTURE_1D,
        0,
        GL_ALPHA,
        4,
        0,
        GL_ALPHA,
        GL_UNSIGNED_BYTE,
        pixels
        );
}

value ml_fz_version (void);
value ml_fz_version (void)
{
    return caml_copy_string (FZ_VERSION);
}

value ml_llpp_version (void);
value ml_llpp_version (void)
{
    extern char llpp_version[];
    return caml_copy_string (llpp_version);
}

void ml_init (value csock_v, value params_v);
void ml_init (value csock_v, value params_v)
{
    CAMLparam2 (csock_v, params_v);
    CAMLlocal2 (trim_v, fuzz_v);
    int ret;
    int texcount;
    char *fontpath;
    int colorspace;
    int mustoresize;

    state.csock         = Int_val (csock_v);
    state.rotate        = Int_val (Field (params_v, 0));
    state.fitmodel      = Int_val (Field (params_v, 1));
    trim_v              = Field (params_v, 2);
    texcount            = Int_val (Field (params_v, 3));
    state.sliceheight   = Int_val (Field (params_v, 4));
    mustoresize         = Int_val (Field (params_v, 5));
    colorspace          = Int_val (Field (params_v, 6));
    fontpath            = String_val (Field (params_v, 7));

#ifdef CIDER
    state.utf8cs = 1;
#else
    /* http://www.cl.cam.ac.uk/~mgk25/unicode.html */
    if (setlocale (LC_CTYPE, "")) {
        const char *cset = nl_langinfo (CODESET);
        state.utf8cs = !strcmp (cset, "UTF-8");
    }
    else {
        printd ("emsg setlocale: %d:%s", errno, strerror (errno));
    }
#endif

    if (caml_string_length (Field (params_v, 8)) > 0) {
        state.trimcachepath = ystrdup (String_val (Field (params_v, 8)));

        if (!state.trimcachepath) {
            printd ("emsg failed to strdup trimcachepath: %d:%s",
                    errno, strerror (errno));
        }
    }

    state.ctx = fz_new_context (NULL, NULL, mustoresize);
    fz_register_document_handlers (state.ctx);

    state.trimmargins = Bool_val (Field (trim_v, 0));
    fuzz_v            = Field (trim_v, 1);
    state.trimfuzz.x0 = Int_val (Field (fuzz_v, 0));
    state.trimfuzz.y0 = Int_val (Field (fuzz_v, 1));
    state.trimfuzz.x1 = Int_val (Field (fuzz_v, 2));
    state.trimfuzz.y1 = Int_val (Field (fuzz_v, 3));

    set_tex_params (colorspace);

    if (*fontpath) {
        state.face = load_font (fontpath);
    }
    else {
        int len;
        const unsigned char *data;

        data = pdf_lookup_substitute_font (state.ctx, 0, 0, 0, 0, &len);
        state.face = load_builtin_font (data, len);
    }
    if (!state.face) _exit (1);

    realloctexts (texcount);
    setuppbo ();
    makestippletex ();

    ret = pthread_create (&state.thread, NULL, mainloop, NULL);
    if (ret) {
        errx (1, "pthread_create: %s", strerror (ret));
    }

    CAMLreturn0;
}

#if FIXME || !FIXME
static void UNUSED_ATTR NO_OPTIMIZE_ATTR refmacs (void) {}
#endif
