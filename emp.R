emp = function(X)
{
	s = 0;
	for(i in 1:length(X))
	{
		s = s + X[i];
	}
	H = s/length(X);
	return(H)
}