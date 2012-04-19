package org.jboss.tools.runtime.as.ui.bot.test.matcher;

import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Server;

public class ServerMatcher extends TypeSafeMatcher<Server>{

	private Server expected;
	
	public ServerMatcher(Server expected) {
		this.expected = expected;
	}
	
	@Override
	public boolean matchesSafely(Server item) {
		return expected.equals(item);
	}

	@Override
	public void describeTo(Description description) {
		description.appendValue(expected);
	}
}
