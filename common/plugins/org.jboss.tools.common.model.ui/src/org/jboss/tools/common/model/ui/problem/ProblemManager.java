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

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.jface.action.IAction;

import org.jboss.tools.common.model.ui.ModelUIPlugin;

/**
 * @author Aleksey
 */
public class ProblemManager {
	
	protected ArrayList<Problem> problems = new ArrayList<Problem>();

	public ProblemManager() {}
	
	public Problem addProblem(String message, int line, int column, IAction[] actions) {
		Problem problem = new Problem(message);
		problem.setLine(line);
		problem.setColumn(column);
		problem.addActions(actions);
		return addProblem(problem);
	}

	public Problem addProblem(String message, int line, int column) {
		Problem problem = new Problem(message);
		problem.setLine(line);
		problem.setColumn(column);
		return addProblem(problem);
	}

	public Problem addProblem(String message) {
		Problem problem = new Problem(message);
		return addProblem(problem);
	}
	
	public Problem addProblem(Problem problem) {
		problems.add(problem);
		return problem;
	}
	
	public Iterator iterator() {
		return problems.iterator();
	}

	public int size() {
		return problems.size();
	}

	public void showErrors() {
		Iterator i = problems.iterator();
		while (i.hasNext()) {
			Problem error = (Problem)i.next();
			ProblemDialog dialog = new ProblemDialog(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(), error);
			dialog.open();
		}
	}
	
}
