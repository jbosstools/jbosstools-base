/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.editor.form;

import org.eclipse.jface.viewers.ISelectionChangedListener;

import org.jboss.tools.common.model.ui.forms.ScrollableFormContainer;

public class RightFormContainer extends ScrollableFormContainer {

	private ISelectionChangedListener listener;

	public void setSelectionChangedListener(ISelectionChangedListener listener) {
		this.listener = listener;
	}

	public ISelectionChangedListener getSelectionChangedListener() {
		return listener;
	}

	public void dispose() {
		super.dispose();
		listener = null;
	}
}
