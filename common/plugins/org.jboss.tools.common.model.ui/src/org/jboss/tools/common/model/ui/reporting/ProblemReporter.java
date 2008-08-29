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
package org.jboss.tools.common.model.ui.reporting;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.ui.dialog.ErrorDialog;
import org.jboss.tools.common.reporting.IProblemReporter;

public class ProblemReporter implements IProblemReporter {
	static int DO_NOTNING_ACTION = 0;
	static int ADD_TO_BUFFER_ACTION = 1;
	static int REPORT_ACTION = 2;

	public void reportProblem(IStatus status) {
		
		//R runnable = new R(status);
		/*
		if(isShowProblemDialogOn() && Display.getDefault()!=null) {
			Display.getDefault().syncExec(runnable);
		}
		else {
			runnable.run();
		}
		*/
		if(isShowProblemDialogOn() && Display.getDefault()!=null) {
			showProblemDialog(status);
		}
	}
	
	private boolean isShowProblemDialogOn() {
		return "yes".equals(ReportPreference.SHOW_ERROR_DIALOG_OPTION.getValue());
	}
	
	private boolean isSubmitProblemAutomaticallyOn() {
		return "yes".equals(ReportPreference.SUBMIT_AUTOMATICALLY_OPTION.getValue());
	}
	
	private int showProblemDialog(IStatus status) {
		Shell shell = null;
		shell = ModelPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		return ErrorDialog.openError(shell, "Error", status.getMessage(), status.getException());
	}
	
	public static String throwableToString(String message, Throwable t) {
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			PrintStream ps = new PrintStream(baos);
			if(message == null) message = t.getMessage();
			if(message != null) {
				ps.print(message);
				ps.print("\n");
			}
			t.printStackTrace(ps);
			if((t instanceof SWTError) && (((SWTError)t).throwable != null)) {
				ps.println("\n*** Stack trace of contained exception ***"); //$NON-NLS-1$
				((SWTError)t).throwable.printStackTrace(ps);
			} else if((t instanceof SWTException) && (((SWTException)t).throwable != null)) {
				ps.println("\n*** Stack trace of contained exception ***"); //$NON-NLS-1$
				((SWTException)t).throwable.printStackTrace(ps);
			}
			ps.flush();
			baos.flush();
			return baos.toString();
		} catch (IOException e) {
			//we cannot report here.
			return "Failed to read throwable.";
		}
	}
	
}
