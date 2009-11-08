#include <iostream>
#include <string>
#include <cstdlib>
#include <fstream>
#include <Magick++.h>
using namespace std;
using namespace Magick;

void convert_to_image(int max_iterations, string path_to_file) {
    int width  = 0;
    int height = 0;
    int x = 0;
    int y = 0;
    int iterations = 0;

    ifstream file;
    file.open(path_to_file.c_str());

    if(!file) {
        cerr << "Something went wrong!" << endl;
        exit(1);
    }

    file >> width;
    file >> height;

    Image image(Geometry(width, height), Color(0,0,0,0));
    image.magick("png");
    while(y<height) {
        while(x<width) {
            file >> iterations;
            if(iterations == max_iterations) {
                image.pixelColor(x, y, Color(0,0,0,0));
                cout << x << " " << y << " hit  " << iterations << endl;
            } else {
                int color = iterations*MaxRGB/max_iterations;
                image.pixelColor(x, y, Color(color,color*0.4,color*0.4,0));
                cout << x << " " << y << " hid " << MaxRGB << " " << iterations << endl;
            }
            x++;
        }
        y++;
        x = 0;
    }
    image.write(path_to_file+".png");
}


int main(int argc, char** argv) {

    if(argc==3) {
        int max_iterations = atoi(argv[1]);
        string path_to_file(argv[2]);

        convert_to_image(max_iterations, path_to_file);
    }

    return 0;
}
