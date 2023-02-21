package picture;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;

/**
 * A class that encapsulates and provides a simplified interface for manipulating an image. The
 * internal representation of the image is based on the RGB direct colour model.
 */
public class Picture {

  /**
   * The internal image representation of this picture.
   */
  private final BufferedImage image;

  /**
   * Construct a new (blank) Picture object with the specified width and height.
   */
  public Picture(int width, int height) {
    image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
  }

  /**
   * Construct a new Picture from the image data in the specified file.
   */
  public Picture(String filepath) {
    try {
      image = ImageIO.read(new File(filepath));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Returns true if this Picture is graphically identical to the other one.
   *
   * @param other The other picture to compare to.
   * @return true iff this Picture is graphically identical to other.
   */
  @Override
  public boolean equals(Object other) {
    if (other == null) {
      return false;
    }
    if (!(other instanceof Picture)) {
      return false;
    }

    Picture otherPic = (Picture) other;

    if (image == null || otherPic.image == null) {
      return image == otherPic.image;
    }
    if (image.getWidth() != otherPic.image.getWidth()
        || image.getHeight() != otherPic.image.getHeight()) {
      return false;
    }

    for (int i = 0; i < image.getWidth(); i++) {
      for (int j = 0; j < image.getHeight(); j++) {
        if (image.getRGB(i, j) != otherPic.image.getRGB(i, j)) {
          return false;
        }
      }
    }
    return true;
  }

  /**
   * Return the height of the <tt>Picture</tt>.
   *
   * @return the height of this <tt>Picture</tt>.
   */
  public int getHeight() {
    return image.getHeight();
  }

  /**
   * Return the colour components (red, green, then blue) of the pixel-value located at (x,y).
   *
   * @param x x-coordinate of the pixel value to return
   * @param y y-coordinate of the pixel value to return
   * @return the RGB components of the pixel-value located at (x,y).
   * @throws ArrayIndexOutOfBoundsException if the specified pixel-location is not contained within
   *                                        the boundaries of this picture.
   */
  public Color getPixel(int x, int y) {
    int rgb = image.getRGB(x, y);
    return new Color((rgb >> 16) & 0xff, (rgb >> 8) & 0xff, rgb & 0xff);
  }

  /**
   * Return the width of the <tt>Picture</tt>.
   *
   * @return the width of this <tt>Picture</tt>.
   */
  public int getWidth() {
    return image.getWidth();
  }

  @Override
  public int hashCode() {
    if (image == null) {
      return -1;
    }
    int hashCode = 0;
    for (int i = 0; i < image.getWidth(); i++) {
      for (int j = 0; j < image.getHeight(); j++) {
        hashCode = 31 * hashCode + image.getRGB(i, j);
      }
    }
    return hashCode;
  }

  public void saveAs(String filepath) {
    try {
      ImageIO.write(image, "png", new File(filepath));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Update the pixel-value at the specified location.
   *
   * @param x   the x-coordinate of the pixel to be updated
   * @param y   the y-coordinate of the pixel to be updated
   * @param rgb the RGB components of the updated pixel-value
   * @throws ArrayIndexOutOfBoundsException if the specified pixel-location is not contained within
   *                                        the boundaries of this picture.
   */
  public void setPixel(int x, int y, Color rgb) {

    image.setRGB(x, y,
        0xff000000 | (((0xff & rgb.getRed()) << 16) | ((0xff & rgb.getGreen()) << 8) | (0xff
            & rgb.getBlue())));
  }

  /**
   * Returns a String representation of the RGB components of the picture.
   */
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();

    for (int y = 0; y < getHeight(); y++) {
      for (int x = 0; x < getWidth(); x++) {
        Color rgb = getPixel(x, y);
        sb.append("(");
        sb.append(rgb.getRed());
        sb.append(",");
        sb.append(rgb.getGreen());
        sb.append(",");
        sb.append(rgb.getBlue());
        sb.append(")");
      }
      sb.append("\n");
    }
    sb.append("\n");
    return sb.toString();
  }

  public void invert() {
    int width = getWidth();
    int height = getHeight();

    for (int x = 0; x < width; x++) {
      for (int y = 0; y < height; y++) {
        Color col = getPixel(x, y);

        int r = 255 - col.getRed();
        int g = 255 - col.getGreen();
        int b = 255 - col.getBlue();

        Color newCol = new Color(r, g, b);

        setPixel(x, y, newCol);
      }
    }
  }

  public void grayscale() {
    int width = getWidth();
    int height = getHeight();

    for (int x = 0; x < width; x++) {
      for (int y = 0; y < height; y++) {
        Color col = getPixel(x, y);

        int avg = (col.getRed() + col.getGreen() + col.getBlue()) / 3;

        Color newCol = new Color(avg, avg, avg);

        setPixel(x, y, newCol);
      }
    }
  }

  public Picture rotate(int angle) {
    int width = getWidth();
    int height = getHeight();

    Picture result;

    if (angle == 180) {
      result = new Picture(width, height);
    } else {
      result = new Picture(height, width);
    }

    for (int x = 0; x < width; x++) {
      for (int y = 0; y < height; y++) {
        Color col = getPixel(x, y);

        switch (angle) {
          case 270 -> {
            result.setPixel(y, width - x - 1, col);
          }
          case 180 -> {
            result.setPixel(width - x - 1, height - y - 1, col);
          }
          case 90 -> {
            result.setPixel(height - y - 1, x, col);
          }
          default -> {
            System.err.println("Invalid angle");
          }
        }
      }
    }

    return result;
  }

  public void flipVertical() {
    int width = getWidth();
    int height = getHeight();

    for (int x = 0; x < width; x++) {
      for (int y = 0; y < height / 2; y++) {
        Color col1 = getPixel(x, y);
        Color col2 = getPixel(x, height - y - 1);

        setPixel(x, y, col2);
        setPixel(x, height - y - 1, col1);
      }
    }
  }

  public void flipHorizontal() {
    int width = getWidth();
    int height = getHeight();

    for (int x = 0; x < width / 2; x++) {
      for (int y = 0; y < height; y++) {
        Color col1 = getPixel(x, y);
        Color col2 = getPixel(width - x - 1, y);

        setPixel(x, y, col2);
        setPixel(width - x - 1, y, col1);
      }
    }
  }

  public Picture blur() {
    int width = getWidth();
    int height = getHeight();

    Picture result = new Picture(width, height);

    for (int x = 0; x < width; x++) {
      for (int y = 0; y < height; y++) {
        if (x == 0 || y == 0 || x == width - 1 || y == height - 1) {
          result.setPixel(x, y, getPixel(x, y));
          continue;
        }

        int r = 0;
        int g = 0;
        int b = 0;
        int t = 0;

        for (int dx = -1; dx <= 1; dx++) {
          for (int dy = -1; dy <= 1; dy++) {
            Color col = getPixel(x + dx, y + dy);
            r += col.getRed();
            g += col.getGreen();
            b += col.getBlue();
            t++;
          }
        }

        Color col = new Color(r / t, g / t, b / t);

        result.setPixel(x, y, col);
      }
    }

    return result;
  }

  public static Picture blend(Picture[] pics) {
    int width = pics [0].getWidth();
    int height = pics [0].getHeight();

    for (int i = 0; i < pics.length; i++) {
      width = Math.min(width, pics[i].getWidth());
      height = Math.min(height, pics[i].getHeight());
    }

    Picture result = new Picture(width, height);

    for (int x = 0; x < width; x++) {
      for (int y = 0; y < height; y++) {
        int r = 0;
        int g = 0;
        int b = 0;

        for (int i = 0; i < pics.length; i++) {
          Color nxtCol = pics[i].getPixel(x, y);
          r += nxtCol.getRed();
          g += nxtCol.getGreen();
          b += nxtCol.getBlue();
        }

        Color newCol = new Color(r / pics.length, g / pics.length, b / pics.length);

        result.setPixel(x, y, newCol);
      }
    }

    return result;
  }

  public static Picture mosaic(Picture[] pics, int tileSize) {
    int width = pics [0].getWidth();
    int height = pics [0].getHeight();

    for (int i = 1; i < pics.length; i++) {
      width = Math.min(width, pics[i].getWidth());
      height = Math.min(height, pics[i].getHeight());
    }

    int tileWidth = width / tileSize;
    int tileHeight = height / tileSize;

    Picture result = new Picture(tileWidth * tileSize, tileHeight * tileSize);

    for (int i = 0; i < tileWidth; i++) {
      for (int j = 0; j < tileHeight; j++) {
        int pic = (i + j) % pics.length;
        for (int tx = 0; tx < tileSize; tx++) {
          for (int ty = 0; ty < tileSize; ty++) {
            int x = tx + i * tileSize;
            int y = ty + j * tileSize;

            Color col = pics[pic].getPixel(x, y);

            result.setPixel(x, y, col);
          }
        }
      }
    }

    return result;
  }
}
