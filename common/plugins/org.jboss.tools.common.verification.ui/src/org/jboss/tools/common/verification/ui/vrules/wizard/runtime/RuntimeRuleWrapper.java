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
package org.jboss.tools.common.verification.ui.vrules.wizard.runtime;

import org.jboss.tools.common.verification.vrules.*;
import org.jboss.tools.common.verification.ui.vrules.wizard.VRuleTipFactory;

public class RuntimeRuleWrapper extends RuntimeItemWrapper {
	protected VRule rule;

	public RuntimeRuleWrapper(VRule rule) {
		this.rule = rule;
	}

	public String getPresentation() {
		return rule.getName();
	}

	public Object getObject() {
		return rule;
	}

	public VRule[] getRules() {
		return new VRule[]{rule};
	}
    
	public String getTip() {
		return VRuleTipFactory.getRuleTip(rule, manager.getMinSignificance());
	}
    
}
