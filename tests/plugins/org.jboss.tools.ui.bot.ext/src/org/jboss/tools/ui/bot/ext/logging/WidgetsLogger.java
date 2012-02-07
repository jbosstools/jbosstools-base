package org.jboss.tools.ui.bot.ext.logging;

import org.apache.log4j.Logger;
import org.eclipse.swt.widgets.Widget;
import org.hamcrest.Matcher;

/**
 * Contains methods for convenient print of widgets. 
 * 
 * @author Lucia Jelinkova
 *
 */
public class WidgetsLogger {

	private static final Logger log = Logger.getLogger(WidgetsLogger.class);

	public static void log() {
		log(null);
	}

	public static void log(Matcher<? extends Widget> matcher) {
		log(matcher, null);
	}

	public static void log(Matcher<? extends Widget> matcher, Widget parentWidget) {
		if (log.isInfoEnabled()){
			log.info("--------------- Start of the list of widgets ---------------");
			new WidgetsFinder(parentWidget, matcher, new LogWidgetsVisitor()).find();
			log.info("--------------- End of the list of widgets ---------------");
		} else {
			System.out.println("INFO for " + WidgetsLogger.class + " is not enabled");
		}
	}
}
