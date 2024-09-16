ggplot(data = GCC_ext, aes(x=p_ext, y=GCC, ymin= GCC- err, ymax = GCC + err)) + geom_ribbon(fill="grey80") + geom_point() +geom_line()
