//iwnbb

/*INCLUDES*/
/*
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <dirent.h>
#include <stdlib.h>*/


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
	(void *) address;
	char file_name[256];//256 como maximo
}c_struct;

void vfork_exec(const char * file){
	if(vfork()==0){//esta llamada es de uso responsable, i.e, no debemos realizar nada mas que usar el valor devuelto por el vfork como se hace en *()==0... y llamar a exec o a exit
		exec(file);//ejecuta un proceso con el programa que le pasamos
		exit(127);
	}
}

int crearLista(){
	if(!mkfifo(fifo_out)||!(mkfifo(fifo_in))/*sino funciona cualquiera no vale para nada, bueno el out podria ser el stdout en Caml pero la entrada no*/
		return 0;
	vfork_exec("./list_server");//si lo metes en una funcion te olvidas de preocuparte por no modificar nada despues de vfork()
	if(!(fd_out=open(fifo_out, O_WRONLY, (mode_t) 0666)||!((fd_in=open(fifo_in, O_RDONLY, (mode_t) 0666))
		return 0;
	return 1;
}

int sendLista(const char * option, char * buf){
	if(buf==NULL){
		return write(fd_out, (char*) "da igual lo que escriba");
	}
	return write(fd_out, buf);
}

char * strofint(unsigned long protagonista, int is_unsigned_long){
	char * aux;
	int i;
	if(is_unsigned_long){
		aux = malloc(sizeOf(char)*10);
		for(i=1000000000;i!=0;i-=100000000){
			aux[1000000000/i] = (char) (protagonista/i);
		}
	}
	else{
		aux = malloc(sizeOf(char)*16);
		sprintf(aux, "%016lu", protagonista);
	}
	return aux;
}

c_struct recieveLista(){
	c_struct to_return;
	//CIM code in the middle
	return to_return;
}

char * c_struct_to_str(c_struct * to_be_converted){
	char * str = malloc(sizeOf(char)*500);
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
	str_aux = strofint((unsigned long)to_be_converted->size, 0);
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
		printf("concat of address");
	//concat of address
	str_aux = strofint((unsigned long)to_be_converted->address, 1);//quiero que se le aplique el hex V.2 la respuesta es aunque a nadie le importe no V.3 por eso he cambiado el codigo
	strcat(str,str_aux);
	free(str_aux);

	strcat(str," ");
	if(i)
		printf("concat of name");
	//concat of name
	strcat(str,to_be_converted->file_name);//no compila por el tamaño fijo
	strncat(str,to_be_converted->file_name,256);//to test

	return str;
}

int insertarLista(int * whom, c_struct * to_be_inserted){
	char * buf;
	buf = c_struct_to_str(to_be_inserted);
	switch(*whom){
		case 0 : 
			return sendLista("-i1", buf);
		case 1 : 
			return sendLista("-i2", buf);
		case 2 : 
			return sendLista("-i3", buf);
		case 3 | 5 : 
			return sendLista("-i5", buf);
	}
}

int borrarElementoLista(int * whom, c_struct * to_be_removed, int for_size){
	char * buf;
	buf = c_struct_to_str(to_be_removed);
	switch(*whom+for_size){
		case 0+FOR_SIZE : 
			return sendLista("-s1", buf);
		case 1+FOR_SIZE : 
			return sendLista("-s2", buf);
		case 2+FOR_SIZE : 
			return sendLista("-s3", buf);
		case 0 : 
			return sendLista("-b1", buf);
		case 1 : 
			return sendLista("-b2", buf);
		case 2 : 
			return sendLista("-b3", buf);
	}
}

int mostrarLista(int * whom){
	//char * buf = NULL;
	switch(*whom){
		case 0 : 
			return sendLista("-m1", NULL);
		case 1 : 
			return sendLista("-m2", NULL);
		case 2 : 
			return sendLista("-m3", NULL);
		case 3 | 5 : 
			return sendLista("-m5", NULL);
	}
}

int main(){

}