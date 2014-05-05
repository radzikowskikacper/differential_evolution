/*
 * main.cpp
 *
 *  Created on: 25 kwi 2014
 *      Author: michal
 */

#include "../header/utils.hpp"
#include "../header/bitmap_image.hpp"
#include "boost/lexical_cast.hpp"
/**
 * wywołanie
 * nazwa [punkty na jednostke] [ilosc poziomic]
 * 					[ilosc procentowa rysowanych poziomic z podanych] [nazwa_pliku]
 */
int main(int argc, char* argv[])
{
    using boost::lexical_cast;
    using boost::bad_lexical_cast;

	if (argc != 5)
	{
		std::cerr<<"Niepoprawna ilość argumentów\n";
		utils::help();
		return -1;
	}

	// for test
	std::string file_name = "plik.bmp";
	int points_per_unit = 500;
	double p[4];	// przedziały dziedziny funkcji
	p[0] = -2.0;	// x_left
	p[1] = 2.0;		// x_right
	p[2] = 2.0;		// y_top
	p[3] = -2.0;	// y_bottom

	int count_isometric = 0;
	int percent_isometric_drawing = 0;

	try{
		++argv; // wskazuje pierwszy parametr
		points_per_unit = lexical_cast<int>(*argv++);
		count_isometric = lexical_cast<int>(*argv++);
		percent_isometric_drawing = lexical_cast<int>(*argv++);
		file_name = lexical_cast<std::string>(*argv);
	}
	catch (bad_lexical_cast &) {
		std::cerr << "Niepoprawne argumenty:\n";
		utils::help();
		return -1;
	}
	if (!utils::parse_filename(file_name))
	{
		std::cerr<<"niepoprawna nazwa pliku!\n";
		exit(-1);
	}





	std::cout<< "test\n f(0,-1)= "<< utils::goldstein(0.0, -1.0) <<"\n";
	bitmap_image *image;

	try{

	image = utils::generatePointsToBmp(points_per_unit,
			&(utils::goldstein),
			p[0], p[1],
			p[2], p[3],
			count_isometric , percent_isometric_drawing);
	}
	catch ( MyException& e) {
		std::cerr<< e.what();
		exit(-1);
	}

    image->save_image(file_name);



	//end for test
	return 0;
}



