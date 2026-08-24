function CurveMapper5
%
% CurveMapper4
%
% Developed by Chuck Witt for use in the Lauder Lab
% Last Updated:  June 21, 2011
%
% Use to trace and follow a 2D curve through frames in a video sequence.
% Three modes available: single frame, multiple frame, periodic motion.
% For help, see accompanying instruction file CurveMapper4.doc.
%
%

close all
clear all
clc

% ----------------------------------------------------------
% Settings structure declaration
% ----------------------------------------------------------

s= struct( ...
    'Mode',1, ...
    'FileName', [], ...
    'PathName', [], ...
    'OutputFileName', [], ...
    'OutputPathName', [], ...
    'MovObj', [], ...
    'UnitConversion', 1, ... Unit conversion initialized to "on"
    'CurveLength', [], ...
    'NumberPoints',[], ...
    'FrameNumber', [], ...
    'StartFrame', [], ... Used by both multiple frame and periodic modes
    'Increment', [], ...
    'EndFrame', [], ...
    'Frequency', [], ...
    'FrameRate', [], ...
    'CurvesPerCycle', [], ...
    'ResamplePoints', 200, ... Adjusts the resample density
    'TimingError', 0 ... Corrects 2.4% timing error when set to 1
    );


% ----------------------------------------------------------
% GUI creation
% ----------------------------------------------------------


% Figure
h_figure= figure(...
    'Position',[100 150 600 600], ...
    'Resize','off', ...
    'Menubar','none', ...
    'NumberTitle','off', ...
    'Name','CurveMapper4', ...
    'Color',[153/255 179/255 204/255], ...
    'Visible','off' ...
    );

% Mode menu
h_mode_menu= uicontrol(h_figure, ...
    'Style','popupmenu', ...
    'Position',[200 400 200 180], ...
    'String',{'Choose Mode...','Single Frame','Multiple Frame','Periodic Motion'}, ...
    'FontSize',13, ...
    'Value',s.Mode, ...
    'Callback',@h_mode_menu_Callback ...
    );

% File choice panel
h_file_panel= uipanel(h_figure, ...
    'Units','pixels', ...
    'Position',[50 460 500 75] ...
    );
h_file_frame= uicontrol(h_file_panel, ...
    'Style','frame', ...
    'Position',[24 41 452 22] ...
    );
h_file_display= uicontrol(h_file_panel, ...
    'Style','text', ...
    'Position',[25 42 450 20], ...
    'FontSize',10.5, ...
    'HorizontalAlignment','left', ...
    'String',['  ' s.FileName] ...
    );
h_file_button= uicontrol(h_file_panel, ...
    'Style','pushbutton', ...
    'String','Load AVI File', ...
    'Position',[25 10 100 24] ,...
    'Callback',@h_file_button_Callback ...
    );

% Unit conversion panel
h_unit_converstion_panel= uipanel(h_figure, ...
    'Units','pixels', ...
    'Position',[75 408 167 40] ...
    );
h_unit_conversion= uicontrol(h_unit_converstion_panel, ...
    'Style','checkbox', ...
    'Position',[135 12 15 15], ...
    'Value',s.UnitConversion, ...
    'Callback',@h_unit_conversion_Callback ...
    );
h_unit_conversion_txt= uicontrol(h_unit_converstion_panel, ...
    'Style','text', ...
    'Position',[10 9 115 20], ...
    'String','Unit Conversion?', ...
    'FontSize',11 ...
    );

% Curve length panel
h_curve_length_panel= uipanel(h_figure, ...
    'Units','pixels', ...
    'Position',[240 408 285 40] ...
    );
h_curve_length= uicontrol(h_curve_length_panel, ...
    'Style','edit', ...
    'Position',[180 9 60 20], ...
    'FontSize',11, ...
    'Callback',@h_curve_length_Callback ...
    );

h_curve_length_txt= uicontrol(h_curve_length_panel, ...
    'Style','text', ...
    'Position',[25 9 140 20], ...
    'String','Actual Curve Length', ...
    'FontSize',11, ...
    'HorizontalAlignment','center' ...
    );

% Output settings panel
h_output_settings_panel= uipanel(h_figure, ...
    'Units','pixels', ...
    'Position',[25 314 550 81] ...
    );
h_output_path_frame= uicontrol(h_output_settings_panel, ...
    'Style','frame', ...
    'Position',[125 42 400 22] ...
    );
h_output_path_display= uicontrol(h_output_settings_panel, ...
    'Style','text', ...
    'Position',[126 43 398 20], ...
    'FontSize',10.5, ...
    'HorizontalAlignment','left' ...
    );
h_output_path_button= uicontrol(h_output_settings_panel, ...
    'Style','pushbutton', ...
    'String','Output Directory', ...
    'Position',[25 41 90 24] ,...
    'Callback',@h_output_path_button_Callback ...
    );
h_output_filename= uicontrol(h_output_settings_panel, ...
    'Style','edit', ...
    'Position',[125 10 400 22], ...
    'FontSize',11, ...
    'HorizontalAlignment','left', ...
    'Callback',@h_output_filename_Callback ...
    );
h_output_filename_text= uicontrol(h_output_settings_panel, ...
    'Style','text', ...
    'Position',[25 11 87 20], ...
    'FontSize',11, ...
    'FontName','Tacoma', ...
    'HorizontalAlignment','center', ...
    'String','Filename' ...
    );


% Single frame panel
h_panel_sf= uipanel(h_figure, ...
    'Units','pixels', ...
    'Position',[75 95 450 200], ...
    'Visible', 'off' ...
    );
h_panel_sf_txt= uicontrol(h_panel_sf, ...
    'Style','text', ...
    'Units','normalized', ...
    'Position',[.02 .92 .8 .08], ...
    'HorizontalAlignment','left', ...
    'String','Single Frame Acquisition', ...
    'FontSize',8, ...
    'FontAngle','italic' ...
    );
h_frame_number= uicontrol(h_panel_sf, ...
    'Style','edit', ...
    'Position',[190 75 70 40], ...
    'FontSize',14, ...
    'Value',s.FrameNumber, ...
    'Callback',@h_frame_number_Callback ...
    );
h_frame_number_txt= uicontrol(h_panel_sf, ...
    'Style','text', ...
    'Position',[170 125 110 20], ...
    'String','Frame Number', ...
    'FontSize',11, ...
    'HorizontalAlignment','center' ...
    );

% Multiple frame panel
h_panel_mf= uipanel(h_figure,...
    'Units','pixels', ...
    'Position',[75 95 450 200], ...
    'Visible', 'off' ...
    );
h_panel_mf_txt= uicontrol(h_panel_mf, ...
    'Style','text', ...
    'Units','normalized', ...
    'Position',[.02 .92 .8 .08], ...
    'HorizontalAlignment','left', ...
    'String','Multiple Frame Acquisition', ...
    'FontSize',8, ...
    'FontAngle','italic' ...
    );
h_start_frame= uicontrol(h_panel_mf,...
    'Style','edit', ...
    'Position',[50 75 70 40], ...
    'FontSize',14, ...
    'Value',s.StartFrame, ...
    'Callback',@h_start_frame_Callback ...
    );
h_start_frame_txt= uicontrol(h_panel_mf,...
    'Style','text', ...
    'Position',[35 125 100 20], ...
    'String','Start Frame', ...
    'FontSize',11, ...
    'HorizontalAlignment','center' ...
    );
h_increment= uicontrol(h_panel_mf,...
    'Style','edit', ...
    'Position',[190 75 70 40], ...
    'FontSize',14, ...
    'Value',s.Increment, ...
    'Callback',@h_increment_Callback ...
    );
h_increment_txt= uicontrol(h_panel_mf,...
    'Style','text', ...
    'Position',[175 125 100 20], ...
    'String','Increment', ...
    'FontSize',11, ...
    'HorizontalAlignment','center' ...
    );
h_end_frame= uicontrol(h_panel_mf,...
    'Style','edit', ...
    'Position',[330 75 70 40], ...
    'FontSize',14, ...
    'Value',s.EndFrame, ...
    'Callback',@h_end_frame_Callback ...
    );
h_end_frame_txt= uicontrol(h_panel_mf,...
    'Style','text', ...
    'Position',[315 125 100 20], ...
    'String','End Frame', ...
    'FontSize',11, ...
    'HorizontalAlignment','center' ...
    );

% Periodic motion panel
h_panel_periodic= uipanel(h_figure, ...
    'Units','pixels', ...
    'Position',[75 95 450 200], ...
    'Visible', 'off' ...
    );
h_panel_periodic_txt= uicontrol(h_panel_periodic, ...
    'Style','text', ...
    'Units','normalized', ...
    'Position',[.02 .92 .8 .08], ...
    'HorizontalAlignment','left', ...
    'String','Periodic Motion Acquisition', ...
    'FontSize',8, ...
    'FontAngle','italic' ...
    );
h_start_frame= uicontrol(h_panel_periodic,...
    'Style','edit', ...
    'Position',[50 95 70 40], ...
    'FontSize',14, ...
    'Value',s.StartFrame, ...
    'Callback',@h_start_frame_Callback ...
    );
h_start_frame_txt= uicontrol(h_panel_periodic,...
    'Style','text', ...
    'Position',[35 140 100 20], ...
    'String','Start Frame', ...
    'FontSize',11, ...
    'HorizontalAlignment','center' ...
    );
h_frequency= uicontrol(h_panel_periodic,...
    'Style','edit', ...
    'Position',[190 95 70 40], ...
    'FontSize',14, ...
    'Value',s.Frequency, ...
    'Callback',@h_frequency_Callback ...
    );
h_frequency_txt= uicontrol(h_panel_periodic,...
    'Style','text', ...
    'Position',[165 140 120 20], ...
    'String','Frequency (Hz)', ...
    'FontSize',11, ...
    'HorizontalAlignment','center' ...
    );
h_frame_rate= uicontrol(h_panel_periodic,...
    'Style','edit', ...
    'Position',[330 95 70 40], ...
    'FontSize',14, ...
    'Value',s.FrameRate, ...
    'Callback',@h_frame_rate_Callback ...
    );
h_frame_rate_txt= uicontrol(h_panel_periodic,...
    'Style','text', ...
    'Position',[300 140 130 20], ...
    'String','Frame Rate (fps)', ...
    'FontSize',11, ...
    'HorizontalAlignment','center' ...
    );
h_curves_per_cycle= uicontrol(h_panel_periodic,...
    'Style','edit', ...
    'Position',[255 30 70 40], ...
    'FontSize',14, ...
    'Value',s.CurvesPerCycle, ...
    'Callback',@h_curves_per_cycle_Callback ...
    );
h_curves_per_cycle_txt= uicontrol(h_panel_periodic,...
    'Style','text', ...
    'Position',[95 40 140 20], ...
    'String','Curves Per Cycle', ...
    'FontSize',11, ...
    'HorizontalAlignment','right' ...
    );

% Video button
h_video_button= uicontrol(h_figure, ...
    'Style','pushbutton', ...
    'String','Play Video', ...
    'Position',[210 25 70 50], ...
    'Callback',@h_video_button_Callback ...
    );

% Go button
h_go_button= uicontrol(h_figure, ...
    'Style','pushbutton', ...
    'String','Go!', ...
    'FontSize',11, ...
    'Position',[320 25 70 50], ...
    'Callback',@h_go_button_Callback ...
    );


set(h_figure,'Visible','on')


% ----------------------------------------------------------
% Callback functions
% ----------------------------------------------------------

    function h_mode_menu_Callback(hObject,eventdata)
        switch get(hObject,'Value')
            case 1
                set(h_panel_sf,'Visible','off')
                set(h_panel_mf,'Visible','off')
                set(h_panel_periodic,'Visible','off')
                s.Mode= 1;
            case 2
                set(h_panel_sf,'Visible','on')
                set(h_panel_mf,'Visible','off')
                set(h_panel_periodic,'Visible','off')
                s.Mode= 2;
            case 3
                set(h_panel_sf,'Visible','off')
                set(h_panel_mf,'Visible','on')
                set(h_panel_periodic,'Visible','off')
                s.Mode= 3;
            case 4
                set(h_panel_sf,'Visible','off')
                set(h_panel_mf,'Visible','off')
                set(h_panel_periodic,'Visible','on')
                s.Mode= 4;
        end 
    end

    function h_file_button_Callback(hObject,eventdata)
        [FileName PathName]= uigetfile('*.avi','Choose an AVI file...');
        if FileName((length(FileName)-3):length(FileName)) ~= '.avi'
            s.FileName= [];
            s.PathName= [];
            s.OutputFileName= [];
            s.OutputPathName= [];
            s.MovObj= [];
        else
            s.FileName= FileName;
            s.PathName= PathName;
            %MODIFICATION : next line is added to define the name wihtout '.avi'
            [~ ,fileNameNoExt, ~] = fileparts(FileName);
            %MODIFICATION : original line -> s.OutputFileName= [s.FileName '_CURVES.xls']; 
            s.OutputFileName= [fileNameNoExt '.xls'];
            s.OutputPathName = 'C:\Users\fguillaud\Documents\Projet\Acquisition fast start\data_matlab\';
            %MODIFICATION : original line -> s.OutputPathName= s.PathName;
            s.MovObj= VideoReader([PathName FileName]);
        end
        set(h_file_display,'String',['  ' s.PathName s.FileName]);
        set(h_output_path_display,'String',[s.OutputPathName]);
        set(h_output_filename,'String',[s.OutputFileName]);
    end

    function h_unit_conversion_Callback(hObject,eventdata)
        if get(hObject,'Value') == 1
            set(h_curve_length_panel,'Visible','on')
            s.UnitConversion= 1;
        elseif get(hObject,'Value') == 0
            set(h_curve_length_panel,'Visible','off')
            s.UnitConversion= 0;
        end
    end

    function h_curve_length_Callback(hObject,eventdata)
        s.CurveLength= check_input(hObject);
    end

    function h_output_path_button_Callback(hObject,eventdata)
        [PathName]= uigetdir(s.PathName,'Choose the output directory...');
        s.OutputPathName= [PathName '\'];
        set(h_output_path_display,'String',s.OutputPathName);
    end

    function h_output_filename_Callback(hObject,eventdata)
        s.OutputFileName= check_filename_input(hObject);
    end

    function h_frame_number_Callback(hObject,eventdata)
        s.FrameNumber= check_input(hObject);
    end

    function h_start_frame_Callback(hObject,eventdata)
        s.StartFrame= check_input(hObject);
    end

    function h_increment_Callback(hObject,eventdata)
        s.Increment= check_input(hObject);
    end

    function h_end_frame_Callback(hObject,eventdata)
        s.EndFrame= check_input(hObject);
    end

    function h_frequency_Callback(hObject,eventdata)
        s.Frequency= check_input(hObject);
    end

    function h_frame_rate_Callback(hObject,eventdata)
        s.FrameRate= check_input(hObject);
    end

    function h_curves_per_cycle_Callback(hObject,eventdata)
        s.CurvesPerCycle= check_input(hObject);
    end

    function h_video_button_Callback(hObject,eventdata)
        if isempty(s.FileName), return, end
        implay([s.PathName s.FileName])
    end

    function h_go_button_Callback(hObject,eventdata)
        if isempty(s.MovObj), return, end
        if s.UnitConversion
            if isempty(s.CurveLength)
                set(h_curve_length_panel,'Visible','off')
                pause(.1)
                set(h_curve_length_panel,'Visible','on')
                return
            end
        end
        if s.Mode==1, return, end
        x=[]; y=[]; xx=[]; yy=[]; M=[]; frame_num_vector=[];
        minxx=[]; avgyy=0; s_arc=[];
        hFigure= figure('units','normalized','position',[.0 .03 1.0 .94],'MenuBar','none');
        switch s.Mode
            case 2
                if isempty(s.FrameNumber),return, end
                [x y xx yy]= MapFrame(s.MovObj,s.FrameNumber,s.ResamplePoints);
                y=-y; yy=-yy;
                xx=xx-min(xx);
                yy=yy-mean(yy);
                s_arc=arc_length(xx,yy);
                if s.UnitConversion
                    p2c=s.CurveLength/s_arc;
                    xx=xx*p2c; yy=yy*p2c;
                end
                M=[xx' yy'];
                frame_num_vector= [s.FrameNumber NaN];
                figure
                plot(M(:,1),M(:,2))
                axis equal
                export_to_excel([s.OutputPathName s.OutputFileName],frame_num_vector,M);
                %MODIFICATION : added percentage to avoid loading files not needed (jpg and emf)
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')
            case 3
                if isempty(s.StartFrame),return, end
                if isempty(s.Increment),return, end
                if isempty(s.EndFrame),return, end
                for i= 1: (s.EndFrame-s.StartFrame)/s.Increment+1
                    current_frame=s.StartFrame+(i-1)*s.Increment;
                    [x y xx(i,:) yy(i,:)]= MapFrame(s.MovObj,current_frame,s.ResamplePoints);
                    y=-y; yy(i,:)=-yy(i,:);
                    if i==1
                        minxx=min(xx);
                        xx=xx-min(xx);
                    else
                        xx(i,:)=xx(i,:)-minxx;
                        if min(min(xx))<minxx
                            xx=xx+minxx;
                            minxx=min(min(xx));
                            xx=xx-minxx;
                        end
                    end
                    if(i==1), avgy=mean(yy); end
                    yy(i,:)=yy(i,:)-avgyy;
                    yy=yy+avgyy;
                    avgyy=mean(mean(yy));
                    yy=yy-avgyy;
                    s_arc(i)=arc_length(xx(i,:),yy(i,:));
                    M=[];
                    for k= 1:size(xx,1)
                        M= [M xx(k,:)' yy(k,:)'];
                    end
                    if s.UnitConversion
                        p2c=s.CurveLength/mean(s_arc);
                        M=M*p2c;
                    end
                    frame_num_vector= [frame_num_vector current_frame NaN];
                end
                export_to_excel([s.OutputPathName s.OutputFileName],frame_num_vector,M);
                figure
                hold on
                for j=1:2:size(M,2)
                    plot(M(:,j),M(:,j+1))
                end
                hold off
                axis equal
                %MODIFICATION : added percentage to avoid loading files not needed (jpg and emf)
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')
            case 4
                if isempty(s.StartFrame),return, end
                if isempty(s.Frequency),return, end
                if isempty(s.FrameRate),return, end
                if isempty(s.CurvesPerCycle),return, end
                period= 1/s.Frequency;
                if s.TimingError
                    period= period/1.024;
                end
                frames_per_cycle= period*s.FrameRate;
                increment= floor(frames_per_cycle/s.CurvesPerCycle);
                for i= 1:s.CurvesPerCycle
                    current_frame= s.StartFrame+(i-1)*increment;
                    [x y xx(i,:) yy(i,:)]= MapFrame(s.MovObj,current_frame,s.ResamplePoints);
                    frame_num_vector= [frame_num_vector current_frame NaN];
                    s_arc(i)=arc_length(xx(i,:),yy(i,:));
                end
                y=-y; yy= -yy;
                yy=yy-mean(mean(yy));
                xx=xx-min(min(x));
                if s.UnitConversion
                        p2c=s.CurveLength/mean(s_arc);
                        xx=xx*p2c; yy=yy*p2c;
                end
                for k= 1:size(xx,1)
                        M= [M xx(k,:)' yy(k,:)'];
                end
                export_to_excel([s.OutputPathName s.OutputFileName],frame_num_vector,M);
                figure
                hold on
                for j=1:2:size(M,2)
                    plot(M(:,j),M(:,j+1))
                end
                hold off
                axis equal
                %MODIFICATION : added percentage to avoid loading files not needed (jpg and emf)
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.jpg'],'jpg')
                %saveas(gcf,[s.OutputPathName s.OutputFileName '.emf'],'emf')
        end
            
        close(hFigure)
    end



% ----------------------------------------------------------
% Nested functions
% ----------------------------------------------------------

    function validated_user_entry= check_input(hObject)
        user_entry= str2double(get(hObject,'String'));
        if ( isnan(user_entry) || user_entry<=0 )
            user_entry= [];
            set(hObject,'String',[])
        end
        validated_user_entry= user_entry;
    end

    function validated_user_entry= check_filename_input(hObject)
        user_entry= get(hObject,'String');        
        % Notice that special characters need to be "escaped" with a backslash
        if regexp(user_entry,'[^\w\.]','once')
            validated_user_entry= [];
            set(hObject,'String',[])
            return;
        end
        if length(user_entry) == regexpi(user_entry,'\.xls','end')
            validated_user_entry= user_entry;
        else
            validated_user_entry= [user_entry '.xls'];
        end
    end

    function export_to_excel(filename,frame_num,M)
        M= [frame_num; M];
        xlswrite(filename, M)
    end

end


% ----------------------------------------------------------
% Subfunctions
% ----------------------------------------------------------


function [x y xx yy]= MapFrame(mov_obj,frame_num,num_points,hFigure)


image_i= read(mov_obj,frame_num);
% image_i=(image_i)
imshow(image_i)
title1= ['Current Frame Number: ' int2str(frame_num)];
title(title1)
fig_pz;
fig_pz(0);

count= 0;


while(1)
    
    [x_pos y_pos button]= ginput(1);
    count= count+1;

    if isempty(button)
        if count==1 || count==2
            count=count-1;
            continue
        else
            break
        end
    end

    switch button
        case 1  % left click
            if(count~=1 && x_pos==x(count-1) && y_pos==y(count-1))
                count=count-1;
                continue
            end
            t(count)= count;
            x(count)= x_pos;
            y(count)= y_pos;
            [xx yy]= SplineFit(t,x,y,num_points);
            UpdateDisplay(image_i,x,y,xx,yy,title1);
        case 2  % middle click
            if isempty(t), count= count-1; continue; end
            t(count-1)=[];
            x(count-1)=[];
            y(count-1)=[];
            count=count-2;
            [xx yy]= SplineFit(t,x,y,num_points);
            UpdateDisplay(image_i,x,y,xx,yy,title1);
        case 3  % right click
            if isempty(t), count= count-1; continue; end
            t(count-1)=[];
            x(count-1)=[];
            y(count-1)=[];
            count=count-2;
            [xx yy]= SplineFit(t,x,y,num_points);
            UpdateDisplay(image_i,x,y,xx,yy,title1);
        case 105  % 'i' key
            fig_pz(3);  % reversed because image axes run backwards
            count= count-1;
        case 107  % 'k' key
            fig_pz(2);
            count= count-1;
        case 109  % 'm' key
            fig_pz(1)
            count= count-1;
        case 106  % 'j' key
            fig_pz(4)
            count= count-1;
        case 61  % plus key
            fig_pz(1,x_pos,y_pos)
            count= count-1;
        case 45  % minus key
            fig_pz(-1,x_pos,y_pos)
            count= count-1;
        case 48
            fig_pz(0);
            count= count-1;
        otherwise
            count= count-1;
    end

end



% ----------------------------------------------------------
% MapFrame nested functions
% ----------------------------------------------------------

    function UpdateDisplay(image_i,x,y,xx,yy,title_text)

    if (isempty(xx) && isempty(yy))
        imshow(image_i)
        title(title_text)
        hold on
        plot(x,y,'.')
        hold off
        fig_pz;
        return;
    end

    imshow(image_i)
    title(title_text)
    hold on
    plot(x,y,'.',xx,yy);
    hold off
    fig_pz;

    end

    function [xx yy]= SplineFit(t,x,y,num_points)

    if (isempty(t) || length(t)==1)
        xx=[];
        yy=[];
        return; 
    end
    s=[0 cumsum(sqrt(diff(x).^2+diff(y).^2))];
    s=s/max(s);
    ss= linspace(0,1,num_points);
    xx=pchip(s,x,ss);
    yy=spline(s,y,ss);

    end

    function fig_pz(varargin)

    % Settings +++++++++++++++++++++++++++++++++++

    pan_shift_amt= 0.2;  % 20 percent
    zoom_factor= 1.5;

    % End Settings +++++++++++++++++++++++++++++++



    persistent OA_FIG_PZ CA_FIG_PZ

    % Collect original axes, no input arguments
    if (nargin==0 && isempty(OA_FIG_PZ) && isempty(CA_FIG_PZ))
    
      a= xlim;
        b= ylim;
        OA_FIG_PZ= [a(1) a(2) b(1) b(2)];
        CA_FIG_PZ= OA_FIG_PZ;
        return;

    % Scale figure to current axes, no input arguments
    elseif (nargin==0 && ~isempty(OA_FIG_PZ) && ~isempty(CA_FIG_PZ))
        axis([CA_FIG_PZ(1) CA_FIG_PZ(2) CA_FIG_PZ(3) CA_FIG_PZ(4)]);
        return;
    
    % Reset figure to original axes, one input argument
    elseif (nargin==1 && varargin{1}==0)
        axis([OA_FIG_PZ(1) OA_FIG_PZ(2) OA_FIG_PZ(3) OA_FIG_PZ(4)]);
        CA_FIG_PZ= OA_FIG_PZ;
        return
    end

    x_center= 0.5*(CA_FIG_PZ(1)+CA_FIG_PZ(2));
    x_range= CA_FIG_PZ(2)-CA_FIG_PZ(1);
    y_center= 0.5*(CA_FIG_PZ(3)+CA_FIG_PZ(4));
    y_range= CA_FIG_PZ(4)-CA_FIG_PZ(3);

    % Pan, one input argument
    if (nargin==1 && varargin{1}~=0)
    
        pan_direc= varargin{1};
        switch pan_direc
            case 1
                y_center= y_center+pan_shift_amt*y_range;
            case 2
                x_center= x_center+pan_shift_amt*x_range;
            case 3
                y_center= y_center-pan_shift_amt*y_range;
            case 4
                x_center= x_center-pan_shift_amt*x_range;
            otherwise
        end
    
    % Zoom, three input arguments    
    elseif (nargin==3 && (varargin{1}==1 || varargin{1}==-1))
    
        zoom_direc= varargin{1};
        switch zoom_direc
            case 1
                x_center= varargin{2};
                y_center= varargin{3};
                x_range= x_range/zoom_factor;
                y_range= y_range/zoom_factor;
            case -1
                x_center= varargin{2};
                y_center= varargin{3};
                x_range= x_range*zoom_factor;
                y_range= y_range*zoom_factor;
            case 2
                x_range= x_range/zoom_factor;
                y_range= y_range/zoom_factor;
            case -2
                x_range= x_range*zoom_factor;
                y_range= y_range*zoom_factor;
            otherwise
        end
    end
    
        x1= x_center-0.5*x_range;
        x2= x_center+0.5*x_range;
        y1= y_center-0.5*y_range;
        y2= y_center+0.5*y_range;
    
        axis([x1 x2 y1 y2])
        CA_FIG_PZ= [x1 x2 y1 y2];
    
    end


end

function s= arc_length(x,y)

if length(x) ~= length(y)
    disp('ERROR:  Input vectors must be of equal lengths')
    return;
end

L= length(y);

s=0;

for i= 1:L-1
    s= s+sqrt( (y(i)-y(i+1))^2 + (x(i)-x(i+1))^2 );
end

end






