//iwnbb

/*INCLUDES*/

#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <fcntl.h>
#include <dirent.h>
#include <stdlib.h>


/*DEFINES*/
#define fifo_out "fifo_lectura_ESCRITURA"
#define fifo_in "fifo_LECTURA_escritura"
#define FOR_SIZE 100

/*VARIABLES*/
int fd_out;
int fd_in;

/*TIPOS*/
typedef struct {
	size_t a_size;
	time_t timex;
	int shared_key;
	void * address;
	char file_name[256];//256 como maximo
}c_struct;

int vfork_exec(const char * file){
	char * aux[2];
	aux[0]=file;
	aux[1]=NULL;
	int status, pid;
	if(pid=(vfork())==0){//esta llamada es de uso responsable, i.e, no debemos realizar nada mas que usar el valor devuelto por el vfork como se hace en *()==0... y llamar a exec o a exit
		execvp(file, aux);//ejecuta un proceso con el programa que le pasamos
		exit(127);
	}
	//el padre no vuelve en si hasta que se hace un exit o un exec, ahora hago un waitpid para saber si salio el hijo o no
	//si salio devuelvo -1 sino 1
	waitpid(pid,&status, WNOHANG);
	return WIFEXITED(status);
}

int crearLista(){
	if(!mkfifo(fifo_out, (mode_t) 0666)||!(mkfifo(fifo_in, (mode_t) 0666)))/*sino funciona cualquiera no vale para nada, bueno el out podria ser el stdout en Caml pero la entrada no*/
		printf("hay que borrar los pipes despues de usarlos");
	if(vfork_exec("./list_server")==-1)
		return 0;//si lo metes en una funcion te olvidas de preocuparte por no modificar nada despues de vfork()
	if(!(fd_out=open(fifo_out, O_WRONLY, (mode_t) 0666))||!(fd_in=open(fifo_in, O_RDONLY, (mode_t) 0666)))
		return 0;//zero es el nuevo -1
	return 1;
}

char * strofint(unsigned long protagonista, int is_unsigned_long){
	char * aux;
	int i;
	if(is_unsigned_long){
		aux = (char *) malloc(sizeof(char)*10);
		for(i=1000000000;i!=0;i-=100000000){
			aux[1000000000/i] = (char) (protagonista/i);
		}
	}
	else{
		aux = (char *) malloc(sizeof(char)*16);
		sprintf(aux, "%016lu", protagonista);
	}
	return aux;
}

c_struct recieveLista(){
	c_struct to_return;
	//CIM code in the middle
	return to_return;
}

char * c_struct_to_str(char * option, c_struct * to_be_converted){
	char * str = NULL;//si no hay memoria queda NULL
	if((str = (char *) malloc(sizeof(char)*500+strlen(option)))==NULL)
		goto RETURN;
	strcat(str, option);//need option first
	if(to_be_converted==NULL)
	/*
		asm volatile ("mov %%rax, %[str]\n\t"
                        "leave\n\t"
                        "ret"
                     : //nothing
                     :[str] "r" (str));
		//goto RETURN;equivalent
	*/
	goto RETURN;
	char * str_aux;
	int i;
	
#ifndef FINAL
	i = 2;
#endif
#ifdef VERBOSE
	i = 2;
#endif

	if(i)
		printf("freeing after concat of str_aux which stores size converted to char *");
	//freeing after concat of str_aux which stores size converted to char *
	str_aux = strofint((unsigned long)to_be_converted->a_size, 0);
	strcat(str,str_aux);
	free(str_aux);
	
	strcat(str," ");
	if(i)
		printf("freeing after concat of str_aux which stores timex converted to char *");
	//freeing after concat of str_aux which stores timex converted to char *
	str_aux = strofint((unsigned long)to_be_converted->timex, 0);
	strcat(str,str_aux);
	free(str_aux);
	
	strcat(str," ");
	if(i)
		printf("freeing after concat of str_aux which stores shared_key converted to char *");
	//freeing after concat of str_aux which stores shared_key converted to char *
	str_aux = strofint((unsigned long)to_be_converted->shared_key, 0);
	strcat(str,str_aux);
	free(str_aux);
	
	strcat(str," ");
	if(i)
		printf("freeing after concat of str_aux which stores address converted to char *");
	//freeing after concat of str_aux which stores address converted to char *
	str_aux = strofint((unsigned long)to_be_converted->address, 1);//quiero que se le aplique el hex V.2 la respuesta es aunque a nadie le importe no V.3 por eso he cambiado el codigo
	strcat(str,str_aux);
	free(str_aux);

	strcat(str," ");
	if(i)
		printf("concat of name");
	//concat of name
	strcat(str,to_be_converted->file_name);//no compila por el tamaño fijo
	strncat(str,to_be_converted->file_name,256);//to test
RETURN:
	return str;
}

int sendLista(char * buf){
	if(buf==NULL){
		perror("error grave");//no hay memoria disponible para una misera variable como buf
		exit(127);
	}
	return write(fd_out, buf, strlen(buf));
}

int insertarLista(int * whom, c_struct * to_be_inserted){
	char * buf = NULL;
	switch(*whom){
		case 0 : 
			buf = c_struct_to_str("-i1", to_be_inserted);
			return sendLista(buf);
		case 1 : 
			buf = c_struct_to_str("-i2", to_be_inserted);
			return sendLista(buf);
		case 2 : 
			buf = c_struct_to_str("-i3", to_be_inserted);
			return sendLista(buf);
		case 3 | 5 : 
			buf = c_struct_to_str("-i5", to_be_inserted);
			return sendLista(buf);
	}
}

int borrarElementoLista(int * whom, c_struct * to_be_removed, int for_size){
	char * buf = NULL;
	switch(*whom+for_size){
		case 0+FOR_SIZE : 
			buf = c_struct_to_str("-s1", to_be_removed);
			return sendLista(buf);
		case 1+FOR_SIZE : 
			buf = c_struct_to_str("-s2", to_be_removed);
			return sendLista(buf);
		case 2+FOR_SIZE : 
			buf = c_struct_to_str("-s3", to_be_removed);
			return sendLista(buf);
		case 0 : 
			buf = c_struct_to_str("-b1", to_be_removed);
			return sendLista(buf);
		case 1 : 
			buf = c_struct_to_str("-b2", to_be_removed);
			return sendLista(buf);
		case 2 : 
			buf = c_struct_to_str("-b3", to_be_removed);
			return sendLista(buf);
	}
}

int mostrarLista(int * whom){
	//char * buf = NULL;
	switch(*whom){
		case 0 : 
			return sendLista((char *)"-m1");
		case 1 : 
			return sendLista((char *)"-m2");
		case 2 : 
			return sendLista((char *)"-m3");
		case 3 | 5 : 
			return sendLista((char *)"-m5");
	}
}

#define INTENTOS_LISTA 20

int main(){
	int i = INTENTOS_LISTA;
	while(--i || crearLista()==-1);//crearLista i intentos
	if(i==0)
		exit(127);
	i = INTENTOS_LISTA;
	c_struct aux;
	aux.a_size=100;
	aux.timex=1000;
	aux.shared_key=0;
	aux.address=(void *) 134010203920;
	strcpy(aux.file_name, "");
	int * p = malloc(sizeof(int));
	*p=1;
	insertarLista(p,&aux);
}