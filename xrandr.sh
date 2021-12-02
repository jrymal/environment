#!/usr/bin/env bash
set -e

# just use a default mode (code is still available to support custom modes)
# supported but not used cause too small...#gettingTooOld
# laptop_mode="2560x1440"

# Reset all inputs if there is an argument
case "${1}" in
    '')
        # nothing to do
        # sets up in a linear line of monitors
        laptop_orientation="normal"
        hdmi_LCD_orientation="normal"
        dp_LCD_orientation="normal"

        direction=left
        #direction=right
        
        laptop_mode="1920x1080"
        hdmi_LCD_mode="1920x1080"
        dp_LCD_mode="1920x1080"
        #hdmi_LCD_mode="2560x1440"
        #dp_LCD_mode="2560x1440"
        ;;
    'h')
        # home set up
        # set left and then next is lower
        laptop_orientation="normal"
        hdmi_LCD_orientation="normal"
        dp_LCD_orientation="normal"
        
        direction=left
        #direction=right

        laptop_mode="1920x1080"
        hdmi_LCD_mode="1920x1080"
        dp_LCD_mode="1920x1080"
        #hdmi_LCD_mode="2560x1440"
        #dp_LCD_mode="2560x1440"
        ;;
    *)
        # work setup
        laptop_orientation="normal"
        hdmi_LCD_orientation="normal"
        dp_LCD_orientation="normal"
        
        direction=left
        #direction=right

        laptop_mode="1920x1080"
        hdmi_LCD_mode="1920x1080"
        dp_LCD_mode="1920x1080"
        #hdmi_LCD_mode="2560x1440"
        #dp_LCD_mode="2560x1440"
        
        echo "Nothing recogonized, assumed reset...."
        arguments=""
        for input in $( xrandr-tool outputs ); do
            # reset the mode and try to reactivate sleeping monitors
            arguments="${arguments} --output ${input} --auto"
        done
        xrandr ${arguments}
        ;;
esac


xrandrOut=$( xrandr )

# actually do the work of setting the modes
arguments=""
for input in $( xrandr-tool outputs ); do
    if [ -n "$( xrandr-tool resolutions ${input} )" ]; then
        
        # add modes and set the mode for each output
        case ${input} in
            eDP-1*)
                mode=${laptop_mode};
                orientation=${laptop_orientation};;
            HDMI-1*)
                mode=${hdmi_LCD_mode};
                orientation=${hdmi_LCD_orientation};;
            DP-1*)
                mode=${dp_LCD_mode};
                orientation=${dp_LCD_orientation};;
            *)
                echo "Unknown monitor type: ${input}"
        esac

        # ensure that the mode exists
        if [ -z "$( echo  ${xrandrOut} | grep "${mode}" )" ]; then
            case ${mode} in
                "laptop_2560x1440_59.99")
                    xrandr --newmode ${mode}  311.78  2560 2744 3024 3488  1440 1441 1444 1490  -HSync +Vsync;;
                "laptop_1920x1080_60.01")
                    xrandr --newmode ${mode}  172.83  1920 2040 2248 2576  1080 1081 1084 1118  -HSync +Vsync;;
                "dp_LCD_2560x1440_59.95")
                    xrandr --newmode ${mode}  311.57  2560 2744 3024 3488  1440 1441 1444 1490  -HSync +Vsync;;
            esac
            
            # add mode to the input
            xrandr --addmode "${input}" "${mode}"
        fi

        # configure the positioning
        if [ -n "${lastM}" ]; then
            if [ -z "${first}" ]; then
                first=true
            fi
            arguments="${arguments} --output ${input} --mode ${mode} --${direction}-of ${lastM} --rotate ${orientation}"
        else
            # always first
            arguments="--output ${input} --mode ${mode} --rotate ${orientation}"
        fi 

        # set last to set positioning
        lastM=${input}
    fi
done

# Actually set the modes and settings in one line (less screen flickers and time spent)
echo "xrandr ${arguments}"
xrandr ${arguments} 
