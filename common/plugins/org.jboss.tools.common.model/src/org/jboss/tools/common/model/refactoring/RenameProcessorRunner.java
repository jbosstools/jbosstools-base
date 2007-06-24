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
package org.jboss.tools.common.model.refactoring;

import org.eclipse.jdt.internal.corext.refactoring.tagging.*;
import org.eclipse.jdt.internal.ui.refactoring.RefactoringSaveHelper;
import org.eclipse.jdt.internal.ui.refactoring.reorg.*;
import org.eclipse.ltk.core.refactoring.participants.*;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class RenameProcessorRunner {
	
	public static void run(RenameProcessor processor, String name) throws Exception {
		RenameRefactoring refactoring = new RenameRefactoring(processor);
		initialize(refactoring, name, 0);
		Shell shell = ModelPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell();
		RenameUserInterfaceStarter starter = new RenameUserInterfaceStarter();
		RenameRefactoringWizard wizard = new RenameRefactoringWizard(refactoring, "Rename", "", null, "");
		starter.initialize(wizard);
		starter.activate(refactoring, shell, RefactoringSaveHelper.SAVE_ALL);
	}

	private static void initialize(RenameRefactoring refactoring, String newName, int flags) {
		if (refactoring.getProcessor() == null)	return;
		((INameUpdating)refactoring.getAdapter(INameUpdating.class)).setNewElementName(newName);
		IReferenceUpdating reference= (IReferenceUpdating)refactoring.getAdapter(IReferenceUpdating.class);
		if (reference != null) {
			reference.setUpdateReferences(true);
		}
		ITextUpdating text= (ITextUpdating)refactoring.getAdapter(ITextUpdating.class);
		if (text != null) {
			text.setUpdateTextualMatches(true);
		}
	}
	
	public static boolean updateReferences(RefactoringProcessor processor) {
		if(!(processor instanceof IReferenceUpdating)) return true;
		return ((IReferenceUpdating)processor).getUpdateReferences();
	}

}
