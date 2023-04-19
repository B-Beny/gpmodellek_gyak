linearis_regresszio = function(x, y)
{
	# x átlag
	mean_x = 0;
	n = length(x); #= length(y)
	for(i in 1:n)
	{
		mean_x = mean_x + x[i];
	}
	mean_x = mean_x / n;

	# y átlag
	mean_y = 0;
	for(i in 1:n)
	{
		mean_y = mean_y + y[i];
	}
	mean_y = mean_y / n;

	a = 0;
	b = 0;
	sum1 = 0;
	sum2 = 0;
	
	# "a" tört számláló
	for(i in 1:n)
	{
		sum1 = sum1 + ((x[i]-mean_x)*(y[i]-mean_y));
	}
	
	# "a" tört nevező
	for(i in 1:n)
	{
		sum2 = sum2 + ((x[i]-mean_x)^2);
	}
	
	a = sum1 / sum2;

	b = mean_y - (a*mean_x)

	cat("Az a értéke: ",a,"\n");
	cat("A b értéke: ",b,"\n");
	# megjelenítés
	plot(x,y);
	abline(b,a);
}