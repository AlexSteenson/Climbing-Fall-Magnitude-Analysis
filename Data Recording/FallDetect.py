import time
import grovepi
import grove6axis

def swapLight(pinOn, pinOff):
    grovepi.pinMode(pinOn, 'OUTPUT')
    grovepi.digitalWrite(pinOn, 1)  # turn it on
    grovepi.pinMode(pinOff, 'OUTPUT')
    grovepi.digitalWrite(pinOff, 0)  # turn it off

swapLight(4, 3)
windowIter = 3;

pressed = False
while grovepi.digitalRead(2) == 0 | pressed == False:
    if grovepi.digitalRead(2) == 1:
        pressed = True

logging = True

while logging:

    time.sleep(2)
    try:
        swapLight(3, 4)

        grove6axis.init6Axis()  # start it up
        print("x, y, z, time")
        firstVals = list(grove6axis.getAccel())
        startTime = time.time()
        print("%f, %f, %f, %f"%(firstVals[0], firstVals[1], firstVals[2], 0.0))
        index = 1
        while True:
            # End logging 
            if grovepi.digitalRead(2) == 1:
                # Turn red light on and green off
                swapLight(4, 3)
                logging = False
                break
            
            # Get the acceleration Values
            nextVals = list(grove6axis.getAccel())
            # Print values - with piping it'll go to csv
            print("%f, %f, %f, %f"%(nextVals[0], nextVals[1], nextVals[2], time.time() - startTime))

    except IOError:
        working = False
        print("Unable to read from accelerometer, check the sensor and try again")
   