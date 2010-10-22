package org.jboss.tools.ui.bot.ext.matcher;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

import org.eclipse.swt.widgets.Item;
import org.eclipse.swtbot.swt.finder.matchers.AbstractMatcher;
import org.eclipse.swtbot.swt.finder.utils.SWTUtils;
import org.hamcrest.Description;
import org.hamcrest.Matcher;

public class WithItem<T extends Item> extends AbstractMatcher<T> {
	protected final Matcher<?>		itemMatcher;
	protected final ArrayList<T>	matches;

	/**
	 * Matches widgets which contains <code>item(s)</code>, as returned by <code>getItems</code> method, that match
	 * given matcher...i.e CTabFolder with Item with text "xyz"
	 * Note: Code taken from unreleased SWTBot, after release will be deprecated and removed
	 * 
	 * @param itemMatcher the item matcher
	 */
	public WithItem(Matcher<?> itemMatcher) {
		this.itemMatcher = itemMatcher;
		this.matches = new ArrayList<T>();
	}

	public void describeTo(Description description) {
		description.appendText("with item");
		this.itemMatcher.describeTo(description);
	}

	protected boolean doMatch(Object obj) {
		matches.clear();
		try {
			for (T item : getItems(obj)) {
				if (itemMatcher.matches(item))
					matches.add(item);
			}
		} catch (Exception e) {
			// do nothing
		}
		return !matches.isEmpty();
	}

	@SuppressWarnings("unchecked")
	private T[] getItems(Object obj) throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
		return (T[]) SWTUtils.invokeMethod(obj, "getItems");
	}

	public ArrayList<T> getAllMatches() {
		return matches;
	}

	public T get(int index) {
		return matches.get(index);
	}

}
