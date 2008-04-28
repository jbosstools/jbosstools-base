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

import org.jboss.tools.common.verification.vrules.VRule;
import org.jboss.tools.common.verification.ui.vrules.wizard.VRuleTipFactory;

public class RuleWrapper extends ConfigItemWrapper {
	ConfigSignificanceView significance;
	VRule o;
	public RuleWrapper(VRule rule, ConfigSignificanceView significance, ConfigItemWrapper parent) {
		o = rule;
		this.significance = significance;
		isSelectedInitial = o.isEnabled();
		setParent(parent);
	}
	public String getPresentation() {
		return o.getName();
	}
	public boolean isSelected() {
		return o.isEnabled();
	}
	public void cancel() {
		o.setEnabled(isSelectedInitial);
	}
	public void flip() {
		o.setEnabled(!o.isEnabled());
	}
	public void setDefaults() {
		o.setEnabled(true);
	}
	public String getTip() {
		return VRuleTipFactory.getRuleTip(o, significance.getSignificance());
	}
}
