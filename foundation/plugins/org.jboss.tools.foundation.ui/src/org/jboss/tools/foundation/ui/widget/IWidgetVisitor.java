/*******************************************************************************
  * Copyright (c) 2016 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.foundation.ui.widget;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Control;
/**
 * This interface is implemented by objects that visit UI Controls and their children
 * <p>
 * Usage:
 * <pre>
 * class Visitor implements IWidgetVisitor {
 *    public boolean visit(Control control) {
 *       // your code here
 *       return true;
 *    }
 * }
 * Composite composite = ...;
 * new WidgetUtility().accept(composite, new Visitor());
 * </pre>
 * </p>
 * <p>
 * Clients may implement this interface.
 * </p>
 *
 * @see WidgetVisitorUtility#accept(Composite,IWidgetVisitor)
 */
public interface IWidgetVisitor {
	/**
	 * Visits the given control.
	 *
	 * @param control the control to visit
	 * @return <code>true</code> if the control's children should
	 *		be visited; <code>false</code> if they should be skipped
	 * @exception CoreException if the visit fails for some reason.
	 */
	public boolean visit(Control control);
}
