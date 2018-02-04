<?php
// fractal.php - by Hannah Leitheiser
//    Description: Generates the mandlebrot set in red
//        saved in .png image format.
//    run: php moon.php > fractal.png (in bash)
//      (or put it in a website, I guess)
//    demonstration:
//       http://flower.web.runbox.net/fractal/fractal.php
//   requires: php's gd library for image processing
header('Content-Type: image/png');

/* return 0 for points in the Mandlebrot set, iteration count for those known to escape */
function Mandlebrot($x, $y) {

	/* PHP seems to lack native support for complex numbers, so we'll
	   just write the calculation using regular variables.
	   Stuff most of you will have learned in middle school, so I
	   won't try to explain it. */

	$c_r = $x;
	$c_i = $y;
	$z_r = 0.0;
	$z_i = 0.0;
	for($i = 1 ; $i < 33 ; $i++) {
		$z_r2 = ($z_r * $z_r) - ($z_i * $z_i) + $c_r;
		$z_i = (2 * $z_r * $z_i) + $c_i;
		// Not using the intermediate variable had some
		// interesting effects.
		$z_r = $z_r2;
		if( ( ($z_r * $z_r) + ($z_i * $z_i)) > 4) {
			return $i;
			}
		}
	return 0;
	}


$imageSizeX = 1000;
$imageSizeY = 800;

// Create the image.
$image = imagecreatetruecolor($imageSizeX,$imageSizeY);
$bkcolor = imagecolorallocate($image, 0, 0, 0);

/* PHP seems to have no easy way to simply set RGB of pixels.
   It wants to use pallets, so I have to set an array of 
   colors. */
$colors = array();
for($i = 0 ; $i < 33 ; $i++) {
	array_push($colors, imagecolorallocate($image, $i*8, 0, 0));
	}

// Now just set the color of each pixel.
for($x = 0 ; $x < $imageSizeX ; $x++) {
	for($y = 0 ; $y < $imageSizeY ; $y++) {
		imagesetpixel($image, $x,$y, $colors[  Mandlebrot( (float)$x/400.0 - 1.85, (float)$y/400.0-1) ] );
		}
	}

// And done.  Simple as that.
imagepng($image);
imagedestroy($image);
?>
