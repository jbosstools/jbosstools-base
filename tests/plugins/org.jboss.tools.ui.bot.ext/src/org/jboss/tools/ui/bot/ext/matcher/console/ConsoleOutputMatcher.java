package org.jboss.tools.ui.bot.ext.matcher.console;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.waits.ICondition;
import org.eclipse.swtbot.swt.finder.widgets.TimeoutException;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.jboss.tools.ui.bot.ext.SWTBotFactory;
import org.jboss.tools.ui.bot.ext.condition.TaskDuration;

/**
 * Checks if the console contains specified text (and waits for it to appear if necessary). 
 * 
 * @author Lucia Jelinkova
 *
 */
public class ConsoleOutputMatcher extends TypeSafeMatcher<String> {

	private String consoleText;

	private long timeout;

	public ConsoleOutputMatcher() {
		timeout = 0;
	}
	
	public ConsoleOutputMatcher(long timeout) {
		this.timeout = timeout;
	}
	
	public ConsoleOutputMatcher(TaskDuration taskDuration) {
		this.timeout = taskDuration.getTimeout();
	}
	
	@Override
	public boolean matchesSafely(String item) {
		try {
			SWTBotFactory.getBot().waitUntil(new ConsoleContainsTextCondition(item), timeout);
			if (consoleText == null){
				throw new IllegalStateException("No console output present");
			}
			return true;
		} catch (TimeoutException e){
			return false;
		}
	}

	@Override
	public void describeTo(Description description) {
		description.appendText("is in console output, but instead: \n" + consoleText);
	}

	private class ConsoleContainsTextCondition implements ICondition {

		private String expectedText;

		public ConsoleContainsTextCondition(String item) {
			this.expectedText = item;
		}

		@Override
		public boolean test() throws Exception {
			consoleText = SWTBotFactory.getConsole().getConsoleText();
			if (consoleText == null){
				return false;
			}
			return consoleText.contains(expectedText);
		}

		@Override
		public void init(SWTBot bot) {
		}

		@Override
		public String getFailureMessage() {
			return null;
		}
	}
}
