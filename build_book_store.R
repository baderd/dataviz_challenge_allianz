# R function
# 
# author: Daniel M Bader
##########################

build_book_store <- function(
    customer_genre_list=list(
        c("a","b"),
        c("a","b","c")
    ),
    verbose=T
){
    if(verbose){
        print(customer_genre_list)
    }
    
    # init pair matrix with all genres
    all_genres <- sort(unique(unlist(customer_genre_list)))
    bookstore_mat <- matrix(0, 
        nrow=length(all_genres), ncol=length(all_genres), 
        dimnames = list(all_genres, all_genres)
    )
    
    # count presence of individual genres for diagonal entry
    specific_genre_counts <- sapply(customer_genre_list, function(cus){
        all_genres %in% cus
    })
    rownames(specific_genre_counts) <- all_genres
    
    # update bookstore
    for(g in all_genres){
        bookstore_mat[g,g]=rowSums(specific_genre_counts)[[g]]
    }
    
    # compute pair occurence
    all_pairs <- combn(all_genres, 2, simplify = F)
    for(cus in customer_genre_list){
        for(p in all_pairs){
            has_pair <- all(is.element(p, cus))
            # update both pair entries
            if(has_pair){
                bookstore_mat[p[1],p[2]] <- bookstore_mat[p[1],p[2]]+1
                bookstore_mat[p[2],p[1]] <- bookstore_mat[p[2],p[1]]+1
            }
        }
    }
    
    # res
    bookstore_mat
}

build_book_store()
