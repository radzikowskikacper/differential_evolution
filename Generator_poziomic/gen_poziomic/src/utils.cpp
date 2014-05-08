#include "../header/utils.hpp"
#include <cfloat>
#include <exception>
#include <cmath>
#include "boost/filesystem.hpp"
void utils::help()
{
	std::cout<<"Uruchomienie programu:\n"
			"nazwa_prog pts y pcnt nazwa_pliku\n"
			"pts - ilosc pktów na jednostke\n"
			"y - ilość poziomic \n"
			"pcnt - wartosc procentowa od ktorej rysujemy \n"
			"nazwa_pliku - nazwa  tworzonego pliku\n";
}

double utils::goldstein(double x, double y)
{
	return (1+(x+y+1)*(x+y+1)*(19-14*x+3*x*x-14*y+6*x*y+3*y*y)) *
			(30+(2*x-3*y)*(2*x-3*y)*(18-32*x+12*x*x+48*y-36*x*y+27*y*y));
}

bool utils::parse_filename(std::string& filename)
{
	return boost::filesystem::portable_posix_name(filename);
}

bitmap_image* utils::generatePointsToBmp(int points_per_unit,
		double (*fun_calc)(double x, double y),
		double x_left, double x_right,
		double y_top, double y_bottom,
		int count_isometric , int percent_isometric_drawing)
{
	if (points_per_unit < 1)
		throw MyException("Niepoprawna wartość points_per_unit");
	if (percent_isometric_drawing > 100 || percent_isometric_drawing < 0)
		throw MyException("Niepoprawna wartość percent_isometric_drawing - dozwolone [0;100]");
	if (count_isometric < 0)
		throw MyException("Niepoprawna wartość count_isometric - musi być nieujemna");
	if (x_left >= x_right)
		throw MyException("zły przedział x");
	if (y_top <= y_bottom)
		throw MyException("zły przedział y");


	int width = points_per_unit * (x_right - x_left);
	int height = points_per_unit * (y_top - y_bottom);
	bitmap_image* image = new bitmap_image(width,height);

	image->clear(0xffff); // ustaw biały kolor

	double *table[width];

	for (int i=0;i<width;++i)
		table[i] = new double[height];

	double min = DBL_MAX;
	double max = DBL_MIN;

	double step = 1.0 / points_per_unit;


	double x_value = x_left;
	double y_value = y_bottom;

	for (int x=0; x<width; ++x )
	{
		y_value = y_bottom;
		for(int y=0; y<height; ++y)
		{
			table[x][y] = fun_calc(x_value,y_value);

			if (min > table[x][y])
				min = table[x][y];
			if (max < table[x][y])
				max = table[x][y];

			y_value += step;
		}
		x_value += step;
	}
	std::cout << "min: " << min << "\nmax: " << max << "\n";
	// make isometrics

	double value_interval = max - min; // rozpiętość przedziału wyniku
	double top_limes = ( value_interval *
			( 100 - percent_isometric_drawing ) / 100 ) + min; // od kiedy malujemy poziomice

	step = value_interval / ( count_isometric + 1 ); // krok poziomicy

	double begin_value = 0.0;
	{

	int l = ((int)((top_limes - min) / step) + 1) ;

	begin_value = l * step + min;

	// nowa wartość ilośći ruchów
	count_isometric -= l - 1; // warunek >0 w petlach
	}
	double accuracy = 1000.0;
	std::cout<< "min: " << min
			<< "\nmax: " << max
			<< "\ntop limes "<< top_limes
			<< "\nbegin value "<< begin_value
			<< "\n";
	/**
	 * zasada działania
	 * porównuj punkty po kolei i  sprawdzaj z wartośćiami oczekiwanuymi poziomic
	 * jak spasuje wartosc to rozpocznij w dwie strony od punktu liczyć poziomice do momentu kiedy spotkasz punkt początkowy
	 *
	 *
	 */
	for (int x=0; x<width; ++x )
	{
		for(int y=0; y<height; ++y)
		{
			double value = begin_value;
			int count_iso = count_isometric;
			while (count_iso > 0)
			{
				if ( fabs(table[x][y] - value) <= accuracy )
				{ // dopisz punkt do bitmapy i zakoncz jego sprawdzanie
					image->set_pixel(x,height-y-1,0,0,0);

					break;
				}
				value += step;
				--count_iso;
			}
		}
	}

	std::cout<<"koniec obliczeń\n";

	// end - delete this table
	for (int i=0;i<width;++i)
		delete [] table[i];

	return image;
}
