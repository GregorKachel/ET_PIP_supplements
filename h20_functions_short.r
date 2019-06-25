interpol.eyes<-function(xcurve, interval){
  na.block=is.na(xcurve) & !is.na(c(NA, xcurve[-length(xcurve)]))
  na.block=cumsum(na.block)
  na.block[!is.na(xcurve)]=0
  if(is.na(xcurve[length(xcurve)])){na.block[na.block==max(na.block)]=0}
  na.block.len=table(na.block)[-1]
  na.block.to.interp=as.numeric(names(na.block.len)[na.block.len<=interval])
  if(length(na.block.to.interp)>0){
    for (block in 1:length(na.block.to.interp)){
      last=min((1:length(xcurve))[na.block==na.block.to.interp[block]])-1
      xnext=max((1:length(xcurve))[na.block==na.block.to.interp[block]])+1
      slope=(xcurve[xnext]-xcurve[last])/(xnext-last)
      interc=xcurve[last]-slope*last
      xcurve[na.block==na.block.to.interp[block]]=interc+slope*(1:length(xcurve))[na.block==na.block.to.interp[block]]
    }
  }
  return(xcurve)
}
rm(xcurve, interval, na.block, na.block.len, na.block.to.interp, last, xnext, interc, slope)

filter3<-function(xcurve, ch.thresh){
  diffs=abs(xcurve[-length(xcurve)]-xcurve[-1])
  diff.to.subs=c(diffs, NA)
  diff.to.prec=c(NA, diffs)
  ch=apply(cbind(diff.to.subs, diff.to.prec), 1, mean, na.rm=T)
  cutoff=quantile(ch, ch.thresh, na.rm=T)
  ch[is.na(ch)]=0
  xcurve[ch>cutoff]=NA
  return(xcurve)
}
rm(xcurve,ch.thresh, diffs, diff.to.subs, diff.to.prec, ch, cutoff)




