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
package org.jboss.tools.common.verification.ui.vrules.wizard.config;

import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.ui.wizards.query.AbstractQueryWizard;

public class VRulesConfigurationWizard extends AbstractQueryWizard {

	public VRulesConfigurationWizard() {
		try {
			setView(new VRulesConfigurationWizardView());
		} catch (Exception e) {
			ModelPlugin.log(e);
		}
	}

}
