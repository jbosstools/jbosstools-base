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
package org.jboss.tools.common.model.ui.wizards.special;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.operation.*;
import org.eclipse.jface.wizard.ProgressMonitorPart;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

public class ProgressPart implements IRunnableContext {
	DefaultSpecialWizard wizard = null;
	Shell shell = null;
    // The progress monitor
    private ProgressMonitorPart progressMonitorPart;
    private boolean lockedUI = false;
    private Cursor waitCursor;
    private Cursor arrowCursor;
    private long activeRunningOperations = 0;
    private static final String FOCUS_CONTROL = "focusControl"; //$NON-NLS-1$
	
	public ProgressPart(Shell shell, DefaultSpecialWizard wizard) {
		this.shell = shell;
		this.wizard = wizard;
	}
	
	Shell getShell() {
		return shell;
	}

    protected void createProgressMonitorPart(Composite composite) {
        GridLayout pmlayout = new GridLayout();
        pmlayout.numColumns = 1;
        progressMonitorPart = createProgressMonitorPart(composite, pmlayout);
        progressMonitorPart.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
////        progressMonitorPart.setVisible(false);
    }

    private Object aboutToStart(boolean enableCancelButton) {
        Map<Object,Object> savedState = null;
        if (getShell() != null) {
            Control focusControl = getShell().getDisplay().getFocusControl();
            if (focusControl != null && focusControl.getShell() != getShell()) {
                focusControl = null;
            }
            setWaitCursor();
            savedState = saveUIState(true && enableCancelButton);
            saveFocus(savedState, focusControl);
        }
        return savedState;
    }
    private void setWaitCursor() {
        Display d = getShell().getDisplay();
        waitCursor = new Cursor(d, SWT.CURSOR_WAIT);
        setDisplayCursor(waitCursor);
        arrowCursor = new Cursor(d, SWT.CURSOR_ARROW);
    }
    private void saveFocus(Map<Object,Object> state, Control focusControl) {
        if (focusControl != null) {
        	state.put(FOCUS_CONTROL, focusControl);
        }
    }
    

    private void setDisplayCursor(Cursor c) {
        Shell[] shells = getShell().getDisplay().getShells();
        for (int i = 0; i < shells.length; i++)
            shells[i].setCursor(c);
    }

    private Map<Object,Object> saveUIState(boolean keepCancelEnabled) {
        Map<Object,Object> savedState = new HashMap<Object,Object>(10);
//        saveEnableStateAndSet(backButton, savedState, "back", false); //$NON-NLS-1$
//        saveEnableStateAndSet(nextButton, savedState, "next", false); //$NON-NLS-1$
//        saveEnableStateAndSet(finishButton, savedState, "finish", false); //$NON-NLS-1$
//        saveEnableStateAndSet(cancelButton, savedState,
//                "cancel", keepCancelEnabled); //$NON-NLS-1$
//        saveEnableStateAndSet(helpButton, savedState, "help", false); //$NON-NLS-1$
//        if (currentPage != null)
//            savedState.put("page", ControlEnableState.disable(currentPage.getControl())); //$NON-NLS-1$
        return savedState;
    }
    protected ProgressMonitorPart createProgressMonitorPart(
            Composite composite, GridLayout pmlayout) {
        return new ProgressMonitorPart(composite, pmlayout, SWT.DEFAULT) {
            String currentTask = null;

            public void setBlocked(IStatus reason) {
                super.setBlocked(reason);
                if (!lockedUI)//Do not show blocked if we are locking the UI
                    Dialog.getBlockedHandler().showBlocked(getShell(), this, reason,
                            currentTask);
            }

            public void clearBlocked() {
                super.clearBlocked();
                if (!lockedUI)//Do not vlear if we never set it
                	Dialog.getBlockedHandler().clearBlocked();
            }

            public void beginTask(String name, int totalWork) {
                super.beginTask(name, totalWork);
                currentTask = name;
            }

            public void setTaskName(String name) {
                super.setTaskName(name);
                currentTask = name;
            }

            public void subTask(String name) {
                super.subTask(name);
                //If we haven't got anything yet use this value for more context
                if (currentTask == null)
                    currentTask = name;
            }
        };
    }

    public void run(boolean fork, boolean cancelable,
            IRunnableWithProgress runnable) throws InvocationTargetException,
            InterruptedException {
        // The operation can only be canceled if it is executed in a separate thread.
        // Otherwise the UI is blocked anyway.
        Object state = null;
        if (activeRunningOperations == 0)
            state = aboutToStart(fork && cancelable);
        activeRunningOperations++;
        try {
            if (!fork)//If we are not forking do not open other dialogs
                lockedUI = true;
            ModalContext.run(runnable, fork, getProgressMonitor(), getShell()
                    .getDisplay());
            lockedUI = false;
        } finally {
            activeRunningOperations--;
            //Stop if this is the last one
            if (state != null)
                stopped(state);
        }
    }

    protected IProgressMonitor getProgressMonitor() {
        return progressMonitorPart;
    }
    
    

    private void stopped(Object savedState) {
        if (getShell() != null) {
            if (wizard.needsProgressMonitor()) {
////                progressMonitorPart.setVisible(false);
                ////progressMonitorPart.removeFromCancelComponent(cancelButton);
            }
            Map state = (Map) savedState;
            ////restoreUIState(state);
            ////cancelButton.addSelectionListener(cancelListener);
            setDisplayCursor(null);
            ////cancelButton.setCursor(null);
            waitCursor.dispose();
            waitCursor = null;
            arrowCursor.dispose();
            arrowCursor = null;
            Control focusControl = (Control) state.get(FOCUS_CONTROL);
            if (focusControl != null)
                focusControl.setFocus();
        }
    }
    
    public void dispose() {
    	if(progressMonitorPart != null) {
    		progressMonitorPart.dispose();
    		progressMonitorPart = null;
    	}
    }


}
