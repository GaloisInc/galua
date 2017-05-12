class Object

class Canvas
  extends Object

  getDimensions: () -> (number,number)
  getHeight: () -> number
  getWidth: () -> number

  getFilter: () -> (FilterMode,FilterMode)
  setFilter: (FilterMode,FilterMode) -> ()
  setFilter: (FilterMode,FilterMode,number) -> ()

  getFormat: () -> CanvasFormat

  getMSAA: () -> number

  getWrap: () -> (WrapMode, WrapMode)
  setWrap: (WrapMode, WrapMode) -> ()

  newImageData: () -> ImageData
  newImageData: (number,number,number,number) -> ImageData
  renderTo: (() -> ()) -> ()

class Drawable
class Font
class Framebuffer
class Image
class Mesh
class ParticleSystem
class PixelEffect
class Quad
class Shader
class SpriteBatch
class Text
class Texture
class Video

type AlignMode      = center | left | right | justify
type ArcType        = pie | open | closed
type BlendAlphaMode = alphamultiply | premultiplied
type BlendMode      = alpha | replace | screen | add | subtract | multiply | lighten | darken
type CanvasFormat   = normal | hdr | rgba8 | rgba4 | rgb5a1 | rgb565 | rgb10a2 | rgba16f
                    | rgba32f | rg11b10f | srgb
                    | r8 | rg8 | r16f | rg16f | r32f | rg32f


type DrwMode        = fill | line


namespace love
  namespace graphics

    arc : (DrawMode, number, number, number, number, number, number) -> ()
    arc : (DrawMode, ArcType, number, number, number, number, number, number) -> ()

    newCanvas : () -> Canvas
    newCanvas : (number, number) -> Canvas
    newCanvas : (number, number, CanvasFormat) -> Canvas
    newCanvas : (number, number, CanvasFormat, number) -> Canvas

