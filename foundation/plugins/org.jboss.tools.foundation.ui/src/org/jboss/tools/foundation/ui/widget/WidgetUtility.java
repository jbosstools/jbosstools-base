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

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public class WidgetUtility {

	public void enableAllChildren(boolean enabled, Composite composite) {
		doForAllChildren(new IWidgetVisitor() {
			
			@Override
			public void visit(Control control) {
				control.setEnabled(enabled);
			}
		}, composite);
	}
	
	public void doForAllChildren(IWidgetVisitor visitor, Composite composite) {
		if (composite == null
				|| composite.isDisposed()) {
			return;
		}
		for (Control control : composite.getChildren()) {
			if (control instanceof Composite) {
				doForAllChildren(visitor, (Composite) control);
			}
			visitor.visit(control);
		}
	}
}
