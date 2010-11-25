package org.jboss.tools.ui.bot.ext.helper;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.VoidResult;

/**
 * 
 * @author psrna
 *
 */
public class StyledTextHelper {
	
	/**
	 * Mouse click on current caret position
	 */
	public static void mouseClickOnCaret(final StyledText widget){
		
		UIThreadRunnable.syncExec(new VoidResult() {
			
			@Override
			public void run() {
				widget.forceFocus();
				
				int caret_x = widget.getCaret().getLocation().x;
				int caret_y = widget.getCaret().getLocation().y;
				
				// Move mouse
				Event event = new Event();
				event.type = SWT.MouseMove;
				event.x = widget.toDisplay(caret_x, caret_y).x;
				event.y = widget.toDisplay(caret_x, caret_y).y;
				widget.getDisplay().post(event);
				
				// Mouse down
				event = new Event();
				event.type = SWT.MouseDown;
				event.button = 1;
				widget.getDisplay().post(event);
				// Mouse Up
				event = new Event();
				event.type = SWT.MouseUp;
				event.button = 1;
				widget.getDisplay().post(event);
			
			}
		});
	}

}
