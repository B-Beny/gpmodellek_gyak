illeszkedes = function(K,N)
{
	n = length(K);
	chi = qchisq(.95, (n-1));
	sum = 0;
	for(x in 1:n)
	{
		sum = sum + ((K[x]-N*(1/n))^2)/(N*(1/n));
	}
	cat('Szabadsági fok: ',n-1,'\n');
	cat('Kiszámolt érték: ',sum,'\n');
	cat('Viszonyított érték: ',chi,'\n');

	if(sum < chi)
	{
		cat('Elfogadjuk a hipotézist.\n');
	} else
	{
		cat('Nem fogadjuk el.\n');
	}
}