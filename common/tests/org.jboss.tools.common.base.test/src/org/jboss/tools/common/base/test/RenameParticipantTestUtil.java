/*******************************************************************************
  * Copyright (c) 2015 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.base.test;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.RefactoringStatusEntry;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.MoveArguments;
import org.eclipse.ltk.core.refactoring.participants.MoveParticipant;
import org.eclipse.ltk.core.refactoring.participants.RefactoringParticipant;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ltk.core.refactoring.participants.RenameArguments;
import org.eclipse.ltk.core.refactoring.participants.RenameParticipant;
import org.eclipse.ltk.core.refactoring.participants.RenameProcessor;
import org.eclipse.text.edits.MultiTextEdit;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.test.util.JobUtils;
import org.junit.Assert;

public class RenameParticipantTestUtil {
	public static void checkRenameParticipant(IJavaElement element, RenameProcessor renameProcessor,
			RefactoringParticipant participant, String newName, List<TestChangeStructure> changeList)
			throws CoreException {
		processTestChanges(changeList);

		JobUtils.waitForIdle();

		participant.initialize(renameProcessor, element, new RenameArguments(newName, true));
		RefactoringStatus status = participant.checkConditions(new NullProgressMonitor(), null);

		Assert.assertNotNull("Rename participant returned null status", status);

		Assert.assertFalse("There is fatal errors in rename participant", status.hasFatalError());

		CompositeChange rootChange = (CompositeChange) participant.createChange(new NullProgressMonitor());
		
		checkChanges(rootChange, changeList);
	}

	public static void checkRenameProcessor(RenameProcessor processor, List<TestChangeStructure> changeList)
			throws CoreException {
		processTestChanges(changeList);

		JobUtils.waitForIdle();

		// Test before renaming
		checkBeforeRefactoring(changeList);

		// Rename
		RefactoringStatus status = processor.checkInitialConditions(new NullProgressMonitor());

		RefactoringStatusEntry[] entries = status.getEntries();
		for (RefactoringStatusEntry entry : entries) {
			System.out.println("Refactor status - " + entry.getMessage());
		}

		Assert.assertNull("Rename processor returns fatal error",
				status.getEntryMatchingSeverity(RefactoringStatus.FATAL));

		status = processor.checkFinalConditions(new NullProgressMonitor(), null);

		entries = status.getEntries();
		for (RefactoringStatusEntry entry : entries) {
			System.out.println("Refactor status - " + entry.getMessage());
		}

		Assert.assertNull("Rename processor returns fatal error",
				status.getEntryMatchingSeverity(RefactoringStatus.FATAL));

		CompositeChange rootChange = (CompositeChange) processor.createChange(new NullProgressMonitor());

		checkChanges(rootChange, changeList);
	}

	public static void checkMoveParticipant(RefactoringProcessor processor, IResource oldObject,
			IResource destinationObject, MoveParticipant participant, List<TestChangeStructure> changeList)
			throws CoreException {
		processTestChanges(changeList);

		JobUtils.waitForIdle();

		// Test before moving
		checkBeforeRefactoring(changeList);

		// Move
		MoveArguments arguments = new MoveArguments(destinationObject, true);
		boolean initialized = participant.initialize(processor, oldObject, arguments);

		Assert.assertTrue("Participant has not been initialized", initialized);

		RefactoringStatus status = participant.checkConditions(new NullProgressMonitor(), null);

		RefactoringStatusEntry[] entries = status.getEntries();
		for (RefactoringStatusEntry entry : entries) {
			System.out.println("Refactor status - " + entry.getMessage());
		}

		Assert.assertNull("Move processor returns fatal error",
				status.getEntryMatchingSeverity(RefactoringStatus.FATAL));

		CompositeChange rootChange = (CompositeChange) participant.createChange(new NullProgressMonitor());

		Change mainChange = processor.createChange(new NullProgressMonitor());
		mainChange.perform(new NullProgressMonitor());

		checkChanges(rootChange, changeList);
	}

	public static void checkRenameParticipant(RefactoringProcessor processor, IResource oldObject,
			String newName, RenameParticipant participant, List<TestChangeStructure> changeList)
			throws CoreException {
		processTestChanges(changeList);

		JobUtils.waitForIdle(2000);

		// Test before renaming
		checkBeforeRefactoring(changeList);

		// Rename
		RenameArguments arguments = new RenameArguments(newName, true);
		boolean initialized = participant.initialize(processor, oldObject, arguments);

		Assert.assertTrue("Participant has not been initialized", initialized);

		RefactoringStatus status = participant.checkConditions(new NullProgressMonitor(), null);

		RefactoringStatusEntry[] entries = status.getEntries();
		for (RefactoringStatusEntry entry : entries) {
			System.out.println("Refactor status - " + entry.getMessage());
		}

		Assert.assertNull("Rename processor returns fatal error",
				status.getEntryMatchingSeverity(RefactoringStatus.FATAL));

		CompositeChange rootChange = (CompositeChange) participant.createChange(new NullProgressMonitor());

		Change mainChange = processor.createChange(new NullProgressMonitor());
		mainChange.perform(new NullProgressMonitor());

		checkChanges(rootChange, changeList);
	}

	private static void processTestChanges(List<TestChangeStructure> structureList) throws CoreException {
		for (TestChangeStructure tStructure : structureList) {
			IFile file = tStructure.getProject().getFile(tStructure.getFileName());
			String fileContent = FileUtil.readStream(file);
			for (TestTextChange tChange : tStructure.getTextChanges()) {
				if (tChange.getOffset() == -1) {
					int offset = fileContent.indexOf(tChange.getSearchText());
					tChange.setOffset(offset);
				}
			}
		}
	}

	private static void checkBeforeRefactoring(List<TestChangeStructure> changeList) {
		for (TestChangeStructure changeStructure : changeList) {
			IFile file = changeStructure.getProject().getFile(changeStructure.getFileName());
			String content = null;
			try {
				content = readStream(file);
			} catch (CoreException e) {
				e.printStackTrace();
				Assert.fail(e.getMessage());
			}

			for (TestTextChange change : changeStructure.getTextChanges()) {
				Assert.assertNotSame(change.getText(),
						content.substring(change.getOffset(), change.getOffset() + change.getLength()));
			}
		}
	}

	private static void checkChanges(CompositeChange rootChange, List<TestChangeStructure> changeList)
			throws CoreException {
		Assert.assertNotNull("Root change is null", rootChange);

		int numberOfChanges = rootChange.getChildren().length;

		for (int i = 0; i < rootChange.getChildren().length; i++) {
			Change fileChange = rootChange.getChildren()[i];

			MultiTextEdit edit = null;
			IFile file = null;
			if (fileChange instanceof TextFileChange) {
				edit = (MultiTextEdit) ((TextFileChange) fileChange).getEdit();
				file = ((TextFileChange) fileChange).getFile();
				//((JBDSFileChange)fileChange).setSaveMode(TextFileChange.FORCE_SAVE);
			} else if (fileChange instanceof TextFileChange) {
				edit = (MultiTextEdit) ((TextFileChange) fileChange).getEdit();
				file = ((TextFileChange) fileChange).getFile();
			}

			//System.out.println("File - "+fileChange.getFile().getFullPath()+" offset - "+edit.getOffset());

			TestChangeStructure change = findChange(changeList, file);
			if (change != null) {
				Assert.assertEquals(change.size(), edit.getChildrenSize());
			}
		}

		rootChange.perform(new NullProgressMonitor());
		JobUtils.waitForIdle(2000);

		// Test results
		for (TestChangeStructure changeStructure : changeList) {
			IFile file = changeStructure.getProject().getFile(changeStructure.getFileName());
			String content = null;
			content = readStream(file);
			for (TestTextChange change : changeStructure.getTextChanges()) {
				Assert.assertEquals("There is unexpected change in resource - " + file.getName(),
						change.getText(),
						content.substring(change.getOffset(), change.getOffset() + change.getLength()));
			}
		}
		Assert.assertEquals("There is unexpected number of changes", changeList.size(), numberOfChanges);
	}

	public static TestChangeStructure findChange(List<TestChangeStructure> changeList, IFile file) {
		for (TestChangeStructure tcs : changeList) {
			if (tcs.getFileName().equals("/" + file.getFullPath().removeFirstSegments(1).toString()))
				return tcs;
		}
		return null;
	}

	public static IType getJavaType(IProject project, String className) {
		IJavaProject javaProject = EclipseUtil.getJavaProject(project);
		if (javaProject != null) {
			try {
				return javaProject.findType(className);
			} catch (JavaModelException ex) {
				Assert.fail(ex.getMessage());
			}
		}

		return null;
	}

	public static IMethod getJavaMethod(IProject project, String className, String methodName) {
		IType type = getJavaType(project, className);
		if (type != null) {
			return type.getMethod(methodName, new String[0]);
		}
		return null;
	}

	public static IField getJavaField(IProject project, String className, String fieldName) {
		IType type = getJavaType(project, className);
		if (type != null) {
			return type.getField(fieldName);
		}
		return null;
	}

	public static class TestChangeStructure {
		private IProject project;
		private String fileName;
		ArrayList<TestTextChange> textChanges = new ArrayList<TestTextChange>();

		public TestChangeStructure(IProject project, String fileName) {
			this.project = project;
			this.fileName = fileName;
		}

		public IProject getProject() {
			return project;
		}

		public String getFileName() {
			return fileName;
		}

		public ArrayList<TestTextChange> getTextChanges() {
			return textChanges;
		}

		public void addTextChange(TestTextChange change) {
			textChanges.add(change);
		}

		public int size() {
			return textChanges.size();
		}

	}

	public static class TestTextChange {
		private String searchText;
		private int offset;
		private int length;
		private String text;

		public TestTextChange(int offset, int length, String text) {
			this.offset = offset;
			this.length = length;
			this.text = text;
		}

		public TestTextChange(String searchText, int length, String text) {
			this.offset = -1;
			this.searchText = searchText;
			this.length = length;
			this.text = text;
		}

		public void setOffset(int offset) {
			this.offset = offset;
		}

		public String getSearchText() {
			return searchText;
		}

		public int getOffset() {
			return offset;
		}

		public int getLength() {
			return length;
		}

		public String getText() {
			return text;
		}
	}

	public static String readStream(IFile file) throws CoreException {
		String content = null;
		InputStream in = null;
		try {
			in = file.getContents();
			content = readStream(in);
		} finally {
			if (in != null) {
				try {
					in.close();
				} catch (IOException e) {
					e.printStackTrace();
					Assert.fail(e.getMessage());
				}
			}
		}
		return content;
	}

	public static String readStream(InputStream is) {
		StringBuffer sb = new StringBuffer(""); //$NON-NLS-1$
		try {
			byte[] b = new byte[4096];
			while (true) {
				int l = is.read(b, 0, b.length);
				if (l < 0)
					break;
				sb.append(new String(b, 0, l));
			}
			
		} catch (IOException e) {
			e.printStackTrace();
			Assert.fail(e.getMessage());
		} finally {
			try {
				is.close();
			} catch (IOException e) {
				e.printStackTrace();
				Assert.fail(e.getMessage());
			}
		}
		return sb.toString();
	}

}
