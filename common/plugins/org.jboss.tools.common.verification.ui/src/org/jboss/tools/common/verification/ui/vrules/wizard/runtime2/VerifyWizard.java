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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime2;

import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizard;
import org.jboss.tools.common.model.ui.wizards.query.IQueryDialog;

public class VerifyWizard extends AbstractQueryWizard {

	public VerifyWizard() {
		setView(new VerifyWizardView());
	}

	protected IQueryDialog createDialog(Shell shell) {
		return new VerifyDialog(shell);
	}
	
}
