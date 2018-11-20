; 本程序运行在分辨率为 320*200 16色 图形模式下
; 左边框运行贪吃蛇游戏  运行坐标范围 X:[1,21] Y:[0,26]   
; 右边框运行绘制三角形程序 输入数字表示边长   按回车键绘制三角形
; 初始默认运行贪吃蛇游戏  过程中按 Tab 键切换为另一个程序


assume cs:codeseg,ss:stack,ds:data
          
;<---------[0,22]-------->
;--------------------------------------------------
;                        ;                         ;  
;                        ;                         ;      
;                        ;                         ;
;                        ;            ;中点        ;  
;                        ;            (240,100)    ;      
;                        ;                         ;
;                        ;                         ;      
;                        ;                         ;
;                        ;                         ;      
;                        ;                         ;
;--------------------------------------------------
stack segment
    db 128 dup(0)
stack ends
; 贪吃蛇栈
snake_stack segment
    db 128 dup(0)
snake_stack ends

; 三角形栈
trin_stack segment
    db 128 dup(0)
trin_stack ends

data segment
    tip_msg db 'len(10,145):$'

    over db 'Game Over!$'

    point_width dw 7
    point_height dw 7

    snake_head dw 0 ;蛇头坐标  
    body dw 1000 dup(0) ;蛇身坐标
    food dw 0   ;食物坐标
    key db 48h,50h,4bh,4dh; 上下左右扫描码
    direction db 4bh    ;移动方向
;=============================================
; 三角形程序所用变量
    x0 dw 240
    y0 dw 0
    x1 dw 50
    y1 dw 86
    x2 dw 0
    y2 dw 0
    xd dw 0
    yd dw 0
    yi dw 1
    xi dw 1
    ddd dw 0
;==============================================
; 分别保存两个程序栈 ss 和 sp

    s_sp dw 128
    s_ss dw snake_stack

    t_sp dw 110
    t_ss dw trin_stack
;==============================================
    exe db 0ffh     ; 执行标记    ffh : 执行贪吃蛇    0h : 执行三角形
data ends



codeseg segment

start:
    mov ax,stack
    mov ss,ax
    mov sp,128

    mov ax,data
    mov ds,ax


    ; 设置显示模式
    mov ah,0
    mov al,0dh
    int 10h

    mov AH, 0BH
    mov BH, 0
    mov BL,0
    int 10H

    call draw_background
    call init_snake
    call produce_food_check

    call cpy_new_int9
    call save_old_int9
    call set_newint9
 
    jmp init

; 贪吃蛇程序
snake_start:    
    mov ax,0
    mov al,exe
    cmp al,0
    je snake_start
    read_key:
        call process_head
        
        ; 循环实现延时
        mov cx,0Fh
        a:
            push cx
            mov cx,0FFFh
            a1:
                push cx
                call in_key
                pop cx
                loop a1
            pop cx
            loop a

jmp snake_start

; 三角形程序
trin_start:
    call draw_trin
    jmp trin_start


; 初始化代码
; 该代码完成    初始化三角形程序栈空间   开始运行贪吃蛇程序
init:

    ; 初始化三角形程序栈空间 
    mov ax,trin_stack
    mov ss,ax
    mov sp,128
    pushf
    push cs
    mov ax,offset trin_start
    push ax
    push ax

    push bx
    push cx
    push dx
    push si
    push di
    ; 切换为贪吃蛇程序栈
    mov ax,snake_stack
    mov ss,ax
    mov sp,128
    ; 开始运行 贪吃蛇程序
    jmp snake_start

in_key:
    mov al, 0
    mov ah, 1
    int 16h;接收键盘
    cmp ah, 1
    je in_key_end

    mov al, 0
    mov ah, 0
    int 16h;  ;读取按键

    cmp ah,key[0]
    je modify
    cmp ah,key[1]
    je modify
    cmp ah,key[2]
    je modify
    cmp ah,key[3]
    je modify
    jmp in_key_end
; 修改方向标记
modify:
    mov direction,ah
    jmp in_key_end

in_key_end:

ret

process_end:

ret


; 对蛇头进行处理
process_head:
    
    mov ax,0
    mov ah,direction ;判断方向
    cmp ah,key[0]
    je up
    cmp ah,key[1]
    je down 
    cmp ah,key[2]
    je left
    cmp ah,key[3]
    je right
up:
    mov ax,snake_head
    dec ah
    jmp check
down:
    mov ax,snake_head
    inc ah
    jmp check
left:
    mov ax,snake_head
    dec al
    jmp check

right:
    mov ax,snake_head
    inc al
    jmp check
;=======================================
; 游戏结束
game_over:
    call restore_new_int9

    mov ah,2
    mov bh,0
    mov dh,5
    mov dl,5
    int 10h         ;设置光标位置

    mov dx,offset over
    mov ah,9
    int 21h

    mov ax,4c00h
    int 21h
;=======================================
; 检测碰撞
check:
    ; 检查边界

    cmp al,0
    je game_over
    cmp al,22
    je game_over
    cmp ah,0ffh
    je game_over
    cmp ah,27
    je game_over

    ; 检查是否碰到身体

    mov si,0

test_body:
    mov bx,body[si]
    cmp bx,ax
    je game_over
    cmp bx,0

    je test_food
    add si,2
    jmp test_body

test_food:

    ; 检查是否碰到食物
    mov bx,food
    cmp ax,bx
    je eat_food

process_body:
    ; 没有碰到食物
    mov si,2

    ; 清除身体最后一个点
    mov bx,body
    push ax
    mov al,0
    call out_snake_point
    pop ax
    
;移动所有身体
processbody:
    mov bx,body[si]
    cmp bx,0
    je process_body_end
    mov body[si-2],bx
    add si,2
    jmp processbody

    ; 更新蛇头
process_body_end:
    mov bx,snake_head
    mov body[si-2],bx
    mov snake_head,ax
    mov bx,ax
    mov al,7
    call out_snake_point
    jmp process_end
eat_food:
    ; 碰到食物
    mov si,0
; 找到添加身体的位置
eatfood:
    mov bx,body[si]
    cmp bx,0
    je eat_update
    add si,2
    jmp eatfood

; 添加一个身体
eat_update:
    mov bx,snake_head
    mov body[si],bx
    ; 更新蛇头
    mov snake_head,ax

    mov bx,ax
    mov al,7
    call out_snake_point
    ; 重新生成食物
    call produce_food_check
    jmp process_end

;==========================================
; 生成食物并检车
produce_food_check:
; 重新生成食物
reproduce:
    call produce_food
    ; 检查是否在边界上
    cmp bh,0ffh
    je reproduce
    cmp bh,22
    je reproduce

    cmp bl,0    
    je reproduce
    cmp bl,27
    je reproduce

    ; 检查是否在蛇身体上
    mov si,0

check_food:
    mov ax,body[si]
    on_body:
        cmp ax,0
        je on_body_end
        cmp ax,bx
        je reproduce
        add si,2
        jmp check_food
    
on_body_end:
    mov ax,snake_head
    cmp ax,bx
    je reproduce

    ; 食物位置正确  检查结束
    mov food,bx
    ;call show_hex
    ; 绘制食物
    mov al,7
    call out_snake_point

ret
;==========================================
; 产生食物
produce_food:
    push ax
    ; 获取随机数
    mov ax,0
    mov al,22   
    call get_random
    dec al
    mov bh,al

    mov al,26
    call get_random
    mov bl,al
    
    pop ax
   
ret
;==========================================
get_random:
    ;产生从1到AX之间的随机数
    mov dx, 41H 
    out dx, ax  
    in al, dl   ;产生的随机数AL
ret



;==========================================
;   绘制蛇的一个点
;  参数 : bh: 行 bl:列 al:颜色
out_snake_point:
    push bx
    push cx
    push dx
    xor dx,dx
    xor cx,cx

    mov dl,bh
    mov cl,bl

    call draw_snake_point
    pop dx
    pop cx
    pop bx

ret


;  具体绘制蛇的一个点
;  参数  cx:  列   dx: 行   al: 颜色
draw_snake_point:
    push ax
    push bx
    push cx
    push si
    push di
    push ax

    mov bp,sp

    mov ax,point_width
    mul cl
    mov si,ax

    mov di,7
    add di,si

    mov ax,point_height
    mul dl
    mov dx,ax
    add dx,9
    mov bx,point_height
    add bx,dx
    mov ax,ss:[bp]
    add sp,2
drawsnakepoint:    
    call draw_h_line
    inc dx
    cmp dx,bx
    jne drawsnakepoint

    pop di
    pop si
    pop cx
    pop bx
    pop ax

ret

;==========================================
; 绘制 一条水平线
; 参数  si: 起点  di:终点 al:颜色 dx:行
draw_h_line:
    push ax
    push cx
    push si
    push di
    
    mov cx,si

    mov ah, 0ch
    drawhline:
       
        int 10h
        inc cx
        cmp cx,di
        jne drawhline
    pop di
    pop si
    pop cx
    pop ax
ret
;==========================================
; 绘制 一条垂直线
; 参数  si: 起点  di:终点 al:颜色 cx:列
draw_v_line:
    push ax
    push dx
    push si
    push di

    mov dx,si
    mov ah, 0ch
    drawvline:
        int 10h
        inc dx
        cmp dx,di
        jne drawvline
    pop di
    pop si
    pop dx
    pop ax

ret

;==========================================
; 初始化蛇
init_snake:
    push bx
    push cx
    push di

    ; 设置蛇头
    mov bx,0c0ah
    mov snake_head[0],bx
    call out_snake_point

    mov cx,3
    mov di,0
    mov bh,12
    mov bl,13
    mov al,7
    ; 设置身体
initsnake: 
    mov body[di],bx
    push cx
    call out_snake_point
    pop cx
    dec bl
    add di,2
    loop initsnake
    pop di
    pop cx
    pop bx
ret
;==========================================

;绘制背景
draw_background:

    ;上边界
    mov si,0
    mov di,320
    mov al,7
    mov dx,0
    mov cx,9
do0:
    call draw_h_line
    inc dx
    loop do0

    ;下边界
    mov si,0
    mov di,320
    mov dx,194
    mov cx,9
do1:
    call draw_h_line
    inc dx
    loop do1
    

    ;左边界
    mov si,0
    mov di,200
    
    mov bx,0
    mov cx,0
do2:

    call draw_v_line
    inc cx
    inc bx
    cmp bx,7
    jne do2

    ;右边界
    mov bx,0
    mov cx,320-1
do3:
    call draw_v_line
    dec cx
    inc bx
    cmp bx,7
    jne do3 

    ;中间界线


    mov bx,0
    mov cx,154
do4:
    call draw_v_line
    inc cx
    inc bx
    cmp bx,7
    jne do4
ret


;   设置新int9 中断
;==========================================
set_newint9:

    push ax
    push ds
    mov ax,0
    mov ds,ax

    cli
    mov word ptr ds:[4*9],7e00h
    mov word ptr ds:[4*9+2],0
    sti

    pop ds
    pop ax
ret

;  保存旧int9中断
;==========================================
save_old_int9:
    push ax
    push ds
    mov ax,0
    mov ds,ax

    cli
    push ds:[4*9]
    pop ds:[200h]
    push ds:[4*9+2]
    pop ds:[202h]
    sti

    pop ds
    pop ax
ret


;  重写int9 中断
;==========================================
new_int9: jmp newint9


newint9:
    push ax

    in al,60h

    pushf
    call dword ptr cs:[200h]
    ;  按下ESC键   退出程序
    cmp al,01h
    je exit
    
    cmp al,0fh
    jne new_int9_ret
    ;  按下Tab键   切换任务


    cmp exe,0ffh  ; snake 程序在运行
    jne save_trin

    mov ax,ss
    mov s_ss,ax

    push bx
    push cx
    push dx
    push si
    push di

    mov ax,sp
    mov s_sp,ax

    mov ax,t_ss
    mov ss,ax

    mov ax,t_sp
    mov sp,ax

    jmp save_end
;   三角形程序在运行
save_trin:
    ;call clear_buff
    ; 保存寄存器信息在栈中
    mov ax,ss
    mov t_ss,ax

    push bx
    push cx
    push dx
    push si
    push di


    ; 恢复 寄存器
    mov ax,sp
    mov t_sp,ax

    mov ax,s_ss
    mov ss,ax

    mov ax,s_sp
    mov sp,ax

save_end:
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    not byte ptr exe ;切换程序

    mov ah,0
    int 16h ;从键盘缓冲区删除Tab

new_int9_ret:
    pop ax
iret


exit:

    call restore_new_int9

    mov ax,4c00h
    int 21h


restore_new_int9:

    push ax
    push ds
    mov ax,0
    mov ds,ax

    cli
    push ds:[200h]
    pop ds:[4*9]
    push ds:[202h]
    pop ds:[4*9+2]
    sti
    pop ds
    pop ax
ret

new_int9_end: nop

;==========================================



;  copy新的int9 中断到0:7e00 处
;==========================================
cpy_new_int9:
    push ax
    push cx
    push ds
    push es
    push si
    push di
    mov ax,cs
    mov ds,ax
    mov ax,0
    mov es,ax
    mov si,offset new_int9
    mov cx,offset new_int9_end - offset new_int9
    mov di,07e00h
    cld
    rep movsb
    pop di
    pop si
    pop es
    pop ds
    pop cx
    pop ax

ret


clear_buff:

    mov ah,1
    int 16h
    jz clear_buff_ret
    mov ah,0
    int 16h
    jmp clear_buff

clear_buff_ret:

    ret

;==========================================
; 三角形程序入口点
draw_trin:

    mov ah,2
    mov bh,0
    mov dh,0
    mov dl,20
    int 10h         ;设置光标位置

    mov dx,offset tip_msg
    mov ah,9
    int 21h


; input_pre:
;     mov ah,07h
;     int 21h
;     cmp al,13
;     jne input_pre

    call clear_buff

    mov cx,0

    ;输入一个整数表示三角形边长
input:
    mov ah,7
    int 21h

    cmp al,1bh
    je exit
    cmp al,13
    je input_end

    cmp al,30h
    jb input
    cmp al,39h
    ja input
    
    mov ah,2
    mov dl,al
    int 21h

    sub al,30h
    mov ah,0
    
    mov dx,cx

    add cx,cx
    add cx,cx
    add cx,cx

    add cx,dx
    add cx,dx

    add cx,ax
    jmp input

    cmp cx,10
    jb input
    cmp cx,145
    ja input

;计算三角形三个点坐标
input_end:
    push cx

    mov ax,cx
    mov cl,8

    div cl
    mov ah,0
    mov cl,7
    mul cl

    mov ah,0

    mov cl,2
    div cl

    mov ah,0

    mov dx,100
    sub dx,ax
    mov y0,dx

    add dx,ax
    add dx,ax
    mov y1,dx
    mov y2,dx

    mov dx,240
    pop ax
    mov cl,2
    div cl
    mov ah,0
    add dx,ax
    mov x1,dx
    sub dx,ax
    sub dx,ax
    mov x2,dx
    
call clear_trin

;绘制线
    mov si,x2
    mov di,x1
    mov dx,y1    

    call draw_tilt_line

    mov ax,x2
    mov x1,ax
    mov ax,y2
    mov y1,ax
    call draw_tilt_line
    xor ax,ax
    mov al,7
    call draw_h_line
ret
;==========================================

plot_line_high:
    mov ax,x1
    mov bx,x0
    sub ax,bx
    mov xd,ax


    mov ax,y1
    mov bx,y0
    sub ax,bx
    mov yd,ax

    mov xi,1
    cmp xd,0
    jge aaaA
    mov xi,-1
    not word ptr xd
    inc word ptr xd


    aaaA:
        mov ax,xd
        add ax,ax
        mov bx,yd
        sub ax,bx
        mov ddd,ax
        mov cx,x0
        mov dx,y0
    loo:
        MOV   AH, 0CH 
        MOV   AL, 7 
        MOV   BH, 0 
        INT   10H

        cmp ddd,0

        jle bbb

        add cx,xi

        mov ax,yd
        add ax,ax

        sub ddd,ax



        bbb:
            mov ax,xd
            add ax,ax
            add ddd,ax
        inc dx
        cmp dx,y1
        jle loo

ret

; Bresenham's line algorithm 画线
draw_tilt_line:
    
    mov ax,y0
    mov bx,y1
    cmp ax,bx
    jb plot


tilt_change:
    mov ax,x0
    mov bx,x1
    mov x0,bx
    mov x1,ax
    mov ax,y0
    mov bx,y1
    mov y0,bx
    mov y1,ax

plot:
    call plot_line_high
ret



clear_trin:

    mov ax,0

    mov dx,9

    mov si,161
    mov di,313
    mov al,0
; 清屏
cleartrin:
    call draw_h_line
    inc dx
    cmp dx,193
    jle cleartrin

    mov si,0
    mov di,320
    mov al,7
    mov dx,0
    mov cx,9
do5:
    call draw_h_line
    inc dx
    loop do5

ret

codeseg ends

end start