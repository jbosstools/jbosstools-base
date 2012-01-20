/*******************************************************************************
 * Copyright (c) 2007 - 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.widget.field;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Link;
import org.jboss.tools.common.ui.widget.editor.ButtonFieldEditor.ButtonPressedAction;

public class LinkField extends BaseField {

	Link link;
	
	/**
	 * 
	 */
	@Override
	public Control getControl() {
		return link;
	}


	public LinkField(Composite composite, ButtonPressedAction listener) {
		link = new Link(composite,SWT.NONE);
		link.setText(listener.getText());
		link.addSelectionListener(listener);
	}

}
