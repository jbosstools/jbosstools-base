package org.jboss.tools.runtime.as.ui.bot.test.matcher;

import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.jboss.tools.runtime.as.ui.bot.test.entity.Runtime;

public class RuntimeMatcher extends TypeSafeMatcher<Runtime>{

	private Runtime expected;
	
	public RuntimeMatcher(Runtime expected) {
		this.expected = expected;
	}
	
	@Override
	public boolean matchesSafely(Runtime item) {
		return expected.equals(item);
	}

	@Override
	public void describeTo(Description description) {
		description.appendValue(expected);
	}
}
