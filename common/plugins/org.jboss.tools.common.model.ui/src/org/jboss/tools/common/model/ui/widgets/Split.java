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
package org.jboss.tools.common.model.ui.widgets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;

public class Split extends SashForm {

	public Split(Composite parent, int style) {
		super(parent, style);
		SASH_WIDTH = 7;
		setOrientation(SWT.HORIZONTAL);
		setBackground(parent.getBackground());
//		this.bgColor = parent.getBackground();
		if ((style & SWT.VERTICAL) != 0){
			setOrientation(SWT.VERTICAL);
		}
	}
}
