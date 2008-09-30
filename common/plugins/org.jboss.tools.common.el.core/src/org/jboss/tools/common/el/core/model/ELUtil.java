/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.model;

import java.util.List;

import org.jboss.tools.common.el.core.parser.LexicalToken;

public class ELUtil {

	public static ELInstance findInstance(ELModel model, int offset) {
		if(model == null) return null;
		List<ELInstance> is = model.getInstances();
		for (ELInstance i: is) {
			if(i.getStartPosition() >= offset) continue;
			LexicalToken c = i.getCloseInstanceToken();
			if(c != null) {
				if(c.getStart() >= offset) return i;
			} else {
				if(i.getEndPosition() >= offset) return i;
			}
		}
		return null;
	}

	public static ELInvocationExpression findExpression(ELModel model, int offset) {
		ELInvocationExpression result = null;
		int off = -1;
		List<ELInstance> is = model.getInstances();
		for (ELInstance i: is) {
			ELExpression expr = i.getExpression();
			if(expr == null) continue;
			if(expr.getFirstToken().getStart() > offset) continue;
			List<ELInvocationExpression> invs = expr.getInvocations();
			for (ELInvocationExpression inv: invs) {
				if(inv.getStartPosition() <= offset && inv.getEndPosition() >= offset) {
					if(off < inv.getStartPosition()) {
						result = inv;
						off = inv.getStartPosition();
						ELInvocationExpression l = inv.getLeft();
						while(l != null && l.getEndPosition() >= offset) {
							result = l;
							l = l.getLeft();
						}
					}
				}
			}
			
		}
		
		return result;
	}

}
