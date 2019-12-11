for image in "$1"/*.png
do
	name="${image%.png}".bmp
	board="$1/board.png"
	
	if [ "$image" = "$board" ];
	then
		convert "$image" "$name"
	continue
	fi

	convert "$image" -resize 100x100 "$name"
done
