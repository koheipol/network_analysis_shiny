library(shiny)
library(visNetwork)
library(igraph)

shinyServer(function(input, output, session) {
    
    observeEvent(input$file, {
        
        csv_file = reactive(data.table::fread(input$file$datapath))
        output$table1 = renderDataTable(csv_file(), options = list(pageLength = 5))
    
    })
    
    observeEvent(input$submit, {
       
        adj = reactive({
            
            if(input$option == "No"){
                
                csv_file = data.table::fread(input$file$datapath)
                csv_file = na.omit(csv_file)
                name = names(csv_file)
                
                if(length(unique(csv_file[[name[1]]])) <= length(unique((csv_file[[name[2]]])))){
                    
                    adjmat=matrix(0,nrow=length(unique(csv_file[[name[2]]])),
                                  ncol=length(unique(csv_file[[name[2]]])))
                    
                    for(i in 1:length(unique(csv_file[[name[2]]]))){
                        
                        for(j in 1:length(unique(csv_file[[name[2]]]))){
                            
                            w=which(csv_file[[name[1]]]==unique(csv_file[[name[2]]])[i])
                            w1=which(csv_file[[name[2]]]==unique(csv_file[[name[2]]])[j])
                            
                            if(sum(is.element(w,w1))==0){
                                
                                adjmat[i,j]=0
                                
                            }else{
                                
                                adjmat[i,j]=sum(csv_file[intersect(w,w1),3])
                                
                            }
                        }
                    }
                    
                    colnames(adjmat) = unique(csv_file[[name[2]]])
                    rownames(adjmat) = colnames(adjmat)
                    return(adjmat)
                    
                }else{
                    
                    adjmat = matrix(0, nrow = length(unique(csv_file[[name[1]]])),
                                    ncol = length(unique(csv_file[[name[1]]])))
                    
                    for(i in 1:length(unique(csv_file[[name[1]]]))){
                        
                        for(j in 1:length(unique(csv_file[[name[1]]]))){
                            
                            w=which(csv_file[[name[2]]]==unique(csv_file[[name[1]]])[i])
                            w1=which(csv_file[[name[1]]]==unique(csv_file[[name[1]]])[j])
                            
                            if(sum(is.element(w, w1))==0){
                                
                                adjmat[j, i]=0
                                
                            }else{
                                
                                adjmat[j, i]=sum(csv_file[intersect(w, w1), 3])
                            }
                        }
                    }
                    
                    colnames(adjmat) = unique(csv_file[[name[1]]])
                    rownames(adjmat) = colnames(adjmat)
                    return(adjmat)
                    
                }
                
            }else{
                
                adjmat = data.table::fread(input$file$datapath)
                adjmat$V1 = NULL
                adjmat = as.matrix(na.omit(adjmat))
                rownames(adjmat) = colnames(adjmat)
                return(adjmat)
                
            }
            
        })
        
        if(input$option >= 1){
            
            g = graph_from_adjacency_matrix(adj(), mode = input$direct, diag = F, weighted = T)
            
        }else{
            
            g = graph_from_adjacency_matrix(adj(), mode = input$direct, diag = F, weighted = T)
            
        }
        
        if(input$node_option == "closeness centrality"){
            
            V(g)$size = closeness(g, mode = input$closeness_type)*input$node_size
            
        }else if(input$node_option == "degree centrality"){
            
            V(g)$size = degree(g, mode = input$degree_type)*input$node_size
            
        }else if(input$node_option == "eigenvector centrality"){
            
            if(input$direct == "directed"){
                
                V(g)$size = evcent(g, directed = T)$vector*input$node_size
                
            }else{
                
                V(g)$size = evcent(g, directed = F)$vector*input$node_size
                
            }
            
        }else if(input$node_option == "PageRank"){
            
            if(input$direct == "directed"){
                
                V(g)$size = page.rank(g, directed = T, algo = input$pagerank_type)$vector*input$node_size
                
            }else{
                
                V(g)$size = page.rank(g, directed = F, algo = input$pagerank_type)$vector*input$node_size
            }
            
        }else if(input$node_option == "Bonacich's power centrality"){
            
            if(input$direct == "directed"){
                
                V(g)$size = bonpow(g, exponent = 0.2, gmode = "digraph")*input$node_size
                
            }else{
                
                V(g)$size = bonpow(g, exponent = 0.2, gmode = "graph")*input$node_size
                
            }
            
        }else if(input$node_option == "betweeness centrality"){
            
            if(input$direct == "directed"){
                
                V(g)$size = betweenness(g, directed = T)*input$node_size
                
            }else{
                
                V(g)$size = betweenness(g, directed = F)*input$node_size
                
            }
        }
        
        if(input$community_option == "edge betweeness"){
            
            com = edge.betweenness.community(g, directed = ifelse(input$direct=="directed", T, F))
            V(g)$color = com$membership
            
        }else if(input$community_option == "random walk"){
            
            com = walktrap.community(g)
            V(g)$color = com$membership
            
        }else if(input$community_option == "fast greedy"){
            
            com = fastgreedy.community(g)
            V(g)$color = com$membership
            
        }else if(input$community_option == "eigen vector"){
            
            com = leading.eigenvector.community(g)
            V(g)$color = com$membership
            
        }else if(input$community_option == "multilevel"){
            
            com = multilevel.community(g, weights = E(g)$weight)
            V(g)$color = com$membership
            
        }else if(input$community_option == "spinglass"){
            
            com = spinglass.community(g, weights = E(g)$weight, spins = 15)
            V(g)$color = com$membership
            
        }else if(input$community_option == "label propagation"){
            
            com = label.propagation.community(g, weights = E(g)$weight)
            V(g)$color = com$membership
            
        }else{
            
            com = infomap.community(g, e.weights = E(g)$weight)
            V(g)$color = com$membership
            
        }
        
        set.seed(123)
        
        output$plot1 = renderVisNetwork({
                
                visIgraph(g) %>%
                    visOptions(highlightNearest = T, nodesIdSelection = T) %>%
                    visInteraction(keyboard = T) %>%
                    visExport()
    })
        
        output$table2 = renderTable({
            
            print(adj())
            
        }, rownames = T)
        
        output$table3 = renderDataTable({
            
            print(data.frame(name = V(g)$name, size = V(g)$size))
            
        })
        
        com_data = data.frame(membership = com$membership, names = com$names)
        com_data = com_data[order(com_data$membership), ]
        rownames(com_data) = 1:nrow(com_data)
        
        output$table4 = renderDataTable({
            
            print(com_data)
            
        })
        
    })
    
    session$onSessionEnded(function(){
        
        stopApp()
        
    })
})
