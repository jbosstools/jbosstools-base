/*******************************************************************************
  * Copyright (c) 2010 - 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.base.test;

import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ltk.core.refactoring.participants.MoveParticipant;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ltk.core.refactoring.participants.RenameParticipant;
import org.eclipse.ltk.core.refactoring.participants.RenameProcessor;
import org.jboss.tools.common.base.test.RenameParticipantTestUtil.TestChangeStructure;

public class AbstractRefactorTest extends TestCase{
	
	public AbstractRefactorTest(String name){
		super(name);
	}
	
	public static void checkRename(RenameProcessor processor, List<TestChangeStructure> changeList) throws CoreException{
		RenameParticipantTestUtil.checkRenameProcessor(processor, changeList);
	}
	
	public static void checkMove(RefactoringProcessor processor, IResource oldObject, IResource destinationObject, MoveParticipant participant, List<TestChangeStructure> changeList) throws CoreException {
		RenameParticipantTestUtil.checkMoveParticipant(processor, oldObject, destinationObject, participant, changeList);
	}

	public static void checkRename(RefactoringProcessor processor, IResource oldObject, String newName, RenameParticipant participant, List<TestChangeStructure> changeList) throws CoreException {
		RenameParticipantTestUtil.checkRenameParticipant(processor, oldObject, newName, participant, changeList);
	}
}
