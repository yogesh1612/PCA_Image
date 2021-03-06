
#---------Staring Server code--------#

server <- function(input, output,session) {
  #-----Data upload----#
  
  up_img <- reactive({
    req(input$file$datapath)
    OpenImageR::readImage(input$file$datapath)
  })
  
  output$dimss <- renderPrint({
    dim(up_img())
  })
  
  output$pix <- renderPrint({
    prettyNum(length(up_img()),big.mark = ',')
  })
  
  output$img <- renderImage({
     oi <- OpenImageR::readImage(input$file$datapath)
     writeImage(oi, "img.jpeg", quality=100)
     filename <- normalizePath(file.path('./',paste0('img','.jpeg')))
     list(src = filename, width = 400,height = 300)
 }, deleteFile = FALSE)
     

  resized_im <- reactive({
    req(up_img())
    im <- up_img()
    resized_im = resizeImage(im, width = input$w, height = input$h, method = 'bilinear')
    resized_im = img_check(resized_im)
    if(input$bw){
      im_gray = rgb_2gray(resized_im)
      im_gray = img_check(im_gray)
      return(im_gray)
    }else{
      return(resized_im)
    }
  })
  
  output$gray_dimss <- renderPrint({
    dim(resized_im())
  })
  
  output$gray_pix <- renderPrint({
    prettyNum(length(resized_im()),big.mark = ',')
  })
  
  
  output$rs_img <- renderImage({
    oi <- resized_im()
    writeImage(oi, "rs_img.jpeg", quality=100)
    filename <- normalizePath(file.path('./',paste0('rs_img','.jpeg')))
    list(src = filename, width = input$w ,height = input$h)
  }, deleteFile = FALSE)
  
  recv_img <- reactive({
    req(up_img())
    im <- img_check(resized_im())
    show_recov_img(im, input$k)
  })
  
  output$pca_dimss <- renderPrint({
    dim(recv_img()[[2]])
  })
  
  output$pca_pix <- renderPrint({
    prettyNum(length(recv_img()[[2]]),big.mark = ',')
  })
  
  
  output$pca_img <- renderPlot({
    req(up_img())
    # define list of components & run
   # kList = c(1, 2, 3, 4, 5, 10)
       # for (i0 in 1:length(kList)){ show_recov_img(resized_gray, kList[i0])}
    #im <- readImage('rs_img.jpeg')
    plot(recv_img()[[1]], main = paste0("with ", input$k, " components."))  
})
 
  # output$nmf_img <- renderPlot({
  #   req(up_img())
  #   # define list of components & run
  #   # kList = c(1, 2, 3, 4, 5, 10)
  #   # for (i0 in 1:length(kList)){ show_recov_img(resized_gray, kList[i0])}
  #   #im <- readImage('rs_img.jpeg')
  #   im <- img_check(resized_im())
  #   plot(show_recov_nmf(im, input$k), main = paste0("with ", input$k, " components."));   
  # })
  # 
  
}