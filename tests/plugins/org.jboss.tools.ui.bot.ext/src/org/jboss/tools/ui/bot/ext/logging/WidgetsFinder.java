package org.jboss.tools.ui.bot.ext.logging;

import static org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable.syncExec;
import static org.eclipse.swtbot.swt.finder.matchers.WidgetMatcherFactory.widgetOfType;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.hamcrest.Matcher;

/**
 * 
 * Finds all widgets recursively and delegates the concrete action
 * to be performed on the visitor (see GOF Visitor pattern)
 * 
 * @author Lucia Jelinkova
 *
 */
public class WidgetsFinder {

	private Matcher<? extends Widget> matcher;

	private Widget parentWidget;
	
	private WidgetVisitor visitor;
	
	private SWTBot bot;
	
	/**
	 * Finds all widgets for the active shell. 
	 */
	public WidgetsFinder(WidgetVisitor visitor) {
		this(null, visitor);
	}

	/**
	 * Finds all widgets meeting specified matcher's condition for the active shell. 
	 * 
	 * @param matcher
	 */
	public WidgetsFinder(Matcher<? extends Widget> matcher, WidgetVisitor visitor) {
		this(null, matcher, visitor);
	}

	/**
	 * Finds all widgets starting from the specified widget meeting
	 * specified matcher condition. 
	 * 
	 * @param widget
	 * @param matcher
	 */
	public WidgetsFinder(Widget widget, Matcher<? extends Widget> matcher, WidgetVisitor visitor) {
		super();
		this.matcher = matcher;
		this.parentWidget = widget;
		this.visitor = visitor;
	}

	public void find() {
		syncExec(new VoidResult() {

			@Override
			public void run() {
				findWidgetRecursive(getParentWidget(), new ArrayList<Widget>());
			}
		});
	}

	private void findWidgetRecursive(Widget widget, ArrayList<Widget> visited){
		
		if (visited.contains(widget)){
			return;
		} 

		visited.add(widget);
		visitWidget(widget);
		
		if (widget instanceof Composite && ((Composite) widget).getChildren().length == 0){
			return;
		}
		
		List<? extends Widget> children = getBot().widgets(getMatcher(), widget);
		for (Widget child : children){
			findWidgetRecursive(child, visited);
		}
	}
	
	private void visitWidget(Widget widget){
		new VisitableWidget(widget).accept(visitor);
	}
	
	public Matcher<? extends Widget> getMatcher() {
		if (matcher == null){
			matcher = widgetOfType(Widget.class);
		}
		return matcher;
	}

	private Widget getParentWidget() {
		if (parentWidget == null){
			// get active shell from generic bot
			parentWidget = new SWTBot().activeShell().widget;
		}
		return parentWidget;
	}
	
	private SWTBot getBot(){
		if (bot == null){
			// create bot for parent widget
			bot = new SWTBot(getParentWidget());
		}
		return bot;
	}
}
