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
package org.jboss.tools.common.model.ui.problem;

import org.eclipse.jface.action.Action;

/**
 * @author Aleksey
 */
public class ProblemDialogAction extends Action {

	private int dialogId = -1;
	private boolean _default;
	
	public ProblemDialogAction() {
		super();
	}

	public ProblemDialogAction(String text, int id, boolean isDefault) {
		super(text);
		this._default = isDefault;
		this.dialogId = id;
	}

	public int getDialogId() {
		return dialogId;
	}

	public void setDialogId(int i) {
		dialogId = i;
	}

	public boolean isDefault() {
		return _default;
	}

	public void setDefault(boolean b) {
		_default = b;
	}

}
