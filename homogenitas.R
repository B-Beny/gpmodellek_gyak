homogenitas = function(F,G)
{
	r = length(F);
	r_c = length(F);

	if(r != r_c)
	{
		return('A megadott két vektor nem azonos méretű, ezért nem lehet kiszámolni!\n');
	}

	chi = qchisq(.95, (r-1));
	n = 0;
	m = 0;

	for(i in 1:r)
	{
		n = n + F[i]
		m = m + G[i]
	}

	sum = 0;
	for(i in 1:r)
	{
		sum = sum + (((F[i]/n) - (G[i]/m))^2)/(F[i] + G[i]); 
	}

	cat('Szabadsági fok: ',r-1,'\n');
	cat('Kiszámolt érték: ',sum,'\n');
	cat('Viszonyított érték: ',chi,'\n');
	cat('Kezdeti hipotézis: a megadott két vektor azonos.\n');	

	if(sum < chi)
	{
		cat('Elfogadjuk a hipotézist.\n');
	} else
	{
		cat('Nem fogadjuk el.\n');
	}
}