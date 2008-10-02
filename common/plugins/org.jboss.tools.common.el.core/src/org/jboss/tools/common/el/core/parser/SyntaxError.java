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
package org.jboss.tools.common.el.core.parser;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class SyntaxError {
	int position;
	int state;
	String problem;

	public SyntaxError(int position, int state) {
		this.position = position;
		this.state = state;
	}

	public int getState() {
		return state;
	}

	public int getPosition() {
		return position;
	}

	public String getProblem() {
		return problem;
	}

	void setProblem(String problem) {
		this.problem = problem;
	}

}
