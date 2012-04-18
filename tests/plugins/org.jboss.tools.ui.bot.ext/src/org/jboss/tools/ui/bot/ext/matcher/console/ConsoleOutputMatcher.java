package org.jboss.tools.ui.bot.ext.matcher.console;

import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;

/**
 * Checks if the console contains specified text. 
 * 
 * @author Lucia Jelinkova
 *
 */
public class ConsoleOutputMatcher extends TypeSafeMatcher<String> {

	private String consoleText;
	
	@Override
	public boolean matchesSafely(String item) {
		consoleText = SWTBotFactory.getConsole().getConsoleText();
		if (consoleText == null){
			throw new IllegalStateException("No console output present");
		}
		return consoleText.contains(item);
	}

	@Override
	public void describeTo(Description description) {
		description.appendText("is in console output, but instead: \n" + consoleText);
	}
}
