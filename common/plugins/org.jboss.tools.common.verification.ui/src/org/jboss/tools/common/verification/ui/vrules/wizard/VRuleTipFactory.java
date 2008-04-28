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
package org.jboss.tools.common.verification.ui.vrules.wizard;

import org.jboss.tools.common.verification.vrules.*;

public class VRuleTipFactory {

	public static String getRuleTip(VRule rule, int minignificance) {
		VResultFactory f = rule.getResultFactory();
		VResultTemplate[] ts = f.getTemplates();
		if(ts == null) return null;
		StringBuffer sb = new StringBuffer();
		sb.append("<html><body>" + rule.getDescription() + "<br>");
		for (int i = 0; i < ts.length; i++) {
			int si = ts[i].getSignificance();
			sb.append("&nbsp;");
			if(si <= minignificance) sb.append("<font color=\"#7f7f7f\">");
///			sb.append('@'); //works swing only
			sb.append(" - "); //for eclipse only
			sb.append(si).append(' ').append(ts[i].getDescription());
			if(si <= minignificance) sb.append("</font>");
			sb.append("<br>");
		}
		sb.append("</body></html>");
		return sb.toString();
	}

	public static String getRuleTip(VRuleSet set) {
		return ("<html><body>" + set.getDescription() + "</body></html>");
	}

}
