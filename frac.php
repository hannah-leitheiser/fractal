<?php
// moon.php - by Hannah Leitheiser
//    Description: Computes the current phase of the moon, three future phases, and draws
//       the moon and that data on a PNG image to ouput a PNG image.
//       Assumes period behavior of the moon; will not account for 
//       the elipticity or the moon's
//       orbit, solar influences, or any other permutations.
//    run: php moon.php > output.png (in bash)
//      (or put it in a website, I guess)
//    demonstration:
//       http://flower.web.runbox.net/moon.php
//   requires: php's gd library for image processing
//       moon.png   1280x720 background image
//       firstq.png 35x35 first quarter moon icon
//       full.png   35x35 full moon icon
//       lastq.png  35x35 last quarter moon icon
//       new.png    35x35 new moon icon
header('Content-Type: image/png');

function Mandlebrot($x, $y) {
/* return 0 for points in the Mandlebrot set, iteration count for those known to escape */
	$c_r = $x;
	$c_i = $y;
	$z_r = 0.0;
	$z_i = 0.0;
	for($i = 1 ; $i < 33 ; $i++) {
		$z_r2 = ($z_r * $z_r) - ($z_i * $z_i) + $c_r;
		$z_i = (2 * $z_r * $z_i) + $c_i;
		$z_r = $z_r2;
		if( ( ($z_r * $z_r) + ($z_i * $z_i)) > 4) {
			return $i;
			}
		}
	return 0;
	}




$lunarMonthDays=29.5306; // days
$lunarMonth=$lunarMonthDays * 86400; // seconds
// compute periods from a known new moon
$moonPeriods = -(1503343800 - time()) / (29.5306 * 86400);
// computer the phase as a fraction from zero to one.  Zero and one indicating new moon.  
// 0.5 indicating full moon.
$moonPhase = $moonPeriods - floor($moonPeriods);
// Use theta = moonPhase*2pi
// cos(theta) represents one coordinate of a unit circle, or 
// the only relevant coordinate of movement around a circle on edge.  
//The light at the moon's equator can be imagined as such a circle. 
$lightPortion=(1-cos($moonPhase*2*3.141592659))/2;
// Save the phase label.  Demarkations are somewhat arbitrary.  
// I gave 1/16 of the month -- about two days I suppose, to 
// momentary phases such as new moon and full moon.
// background image.  Should contain a moon.
$image = imagecreatetruecolor(1000,800);
$bkcolor = imagecolorallocate($image, 0, 0, 0);

$colors = array();
for($i = 0 ; $i < 33 ; $i++) {
	array_push($colors, imagecolorallocate($image, $i*8, 0, 0));
	}

for($x = 0 ; $x < 1000 ; $x++) {
	for($y = 0 ; $y < 800 ; $y++) {
		//$color = imagecolorallocate($image, Mandlebrot( (float)$x/200.0 - 2, (float)$y/400.0-2)*8, 0, 0);
		//$color = imagecolorallocate($image, 100 + 55*($x%2), 255, 0);

		imagesetpixel($image, $x,$y, $colors[  Mandlebrot( (float)$x/400.0 - 1.85, (float)$y/400.0-1) ] );
		}
	}
imagepng($image);
?>
