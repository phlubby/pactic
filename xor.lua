-- XOR-pattern
function TIC()
t=time()
pattern = (t//999)%3
 for y=0,136 do
  for x=0,240 do
   if pattern==0 then
    pix(x,y,x~y)
   elseif pattern==1 then
    pix(x,y,x&y)
   else -- ==2
    pix(x,y,x|y)
   end
  end;
 end;
end