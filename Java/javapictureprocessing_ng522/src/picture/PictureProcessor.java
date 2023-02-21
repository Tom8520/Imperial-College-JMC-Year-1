package picture;

public class PictureProcessor {

  public static void main(String[] args) {
    String operation = args[0];

    switch (operation) {
      case "invert" -> {
        Picture pic = new Picture(args[1]);
        pic.invert();
        pic.saveAs(args[2]);
      }
      case "grayscale" -> {
        Picture pic = new Picture(args[1]);
        pic.grayscale();
        pic.saveAs(args[2]);
      }
      case "rotate" -> {
        Picture pic = new Picture(args[2]);
        int angle = Integer.parseInt(args[1]);

        Picture newPic = pic.rotate(angle);

        newPic.saveAs(args[3]);
      }
      case "flip" -> {
        Picture pic = new Picture(args[2]);

        if (args[1].equals("V")) {
          pic.flipVertical();
        } else if (args[1].equals("H")) {
          pic.flipHorizontal();
        } else {
          System.err.println("Invalid flip axes");
        }

        pic.saveAs(args[3]);
      }
      case "blend" -> {
        Picture pic = new Picture(args[1]);

        Picture[] pics = new Picture[args.length - 2];
        for (int i = 0; i < args.length - 2; i++) {
          pics[i] = new Picture(args[i + 1]);
        }

        Picture newPic = Picture.blend(pics);

        newPic.saveAs(args[args.length - 1]);
      }
      case "blur" -> {
        Picture pic = new Picture(args[1]);

        Picture newPic = pic.blur();

        newPic.saveAs(args[2]);
      }
      case "mosaic" -> {
        int tileSize = Integer.parseInt(args[1]);

        Picture[] pics = new Picture[args.length - 3];
        for (int i = 2; i < args.length - 1; i++) {
          pics[i - 2] = new Picture(args[i]);
        }

        Picture newPic = Picture.mosaic(pics, tileSize);

        newPic.saveAs(args[args.length - 1]);
      }
      default -> {
        System.err.println("Invalid command");
      }
    }
  }
}
