package org.jboss.tools.ui.bot.ext.logging;

import org.eclipse.swt.widgets.Widget;

/**
 * Visitor for the {@link Widget} objects.
 * 
 * @author Lucia Jelinkova
 *
 */
public interface WidgetVisitor {

	void visit(Widget widget);
	
}
