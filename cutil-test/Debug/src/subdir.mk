################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../src/runtests.c 

OBJS += \
./src/runtests.o 

C_DEPS += \
./src/runtests.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cygwin C Compiler'
	gcc -I"E:\hacks\libcutil\include" -I"E:\hacks\libcutil-test\include" -O0 -g3 -pedantic -Wall -Werror -c -fmessage-length=0 -ansi -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


