package org.jboss.tools.tests;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.RefactoringStatusEntry;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.MoveArguments;
import org.eclipse.ltk.core.refactoring.participants.MoveParticipant;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ltk.core.refactoring.participants.RenameArguments;
import org.eclipse.ltk.core.refactoring.participants.RenameParticipant;
import org.eclipse.ltk.core.refactoring.participants.RenameProcessor;
import org.eclipse.text.edits.MultiTextEdit;
import org.jboss.tools.test.util.JobUtils;

public class AbstractRefactorTest extends TestCase{
	
	public AbstractRefactorTest(String name){
		super(name);
	}
	
	public static void checkRename(RenameProcessor processor, List<TestChangeStructure> changeList) throws CoreException{
		JobUtils.waitForIdle(2000);

		// Test before renaming
		checkBeforeRefactoring(changeList);

		// Rename
		RefactoringStatus status = processor.checkInitialConditions(new NullProgressMonitor());
		
		RefactoringStatusEntry[] entries = status.getEntries();
		for(RefactoringStatusEntry entry : entries){
			System.out.println("Refactor status - "+entry.getMessage());
		}
		
		assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));
		
		status = processor.checkFinalConditions(new NullProgressMonitor(), null);
		
		entries = status.getEntries();
		for(RefactoringStatusEntry entry : entries){
			System.out.println("Refactor status - "+entry.getMessage());
		}
		
		assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));
		
		
		CompositeChange rootChange = (CompositeChange)processor.createChange(new NullProgressMonitor());
		
		checkChanges(rootChange, changeList);
	}
	
	public static void checkMove(RefactoringProcessor processor, IResource oldObject, IResource destinationObject, MoveParticipant participant, List<TestChangeStructure> changeList) throws CoreException {
		JobUtils.waitForIdle(2000);

		// Test before moving
		checkBeforeRefactoring(changeList);

		// Move
		MoveArguments arguments = new MoveArguments(destinationObject, true);
		boolean initialized = participant.initialize(processor, oldObject, arguments);
		
		assertTrue("Participant has not been initialized", initialized);
		
		RefactoringStatus status = participant.checkConditions(new NullProgressMonitor(), null);
		
		RefactoringStatusEntry[] entries = status.getEntries();
		for(RefactoringStatusEntry entry : entries){
			System.out.println("Refactor status - "+entry.getMessage());
		}
		
		assertNull("Move processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));
		
		CompositeChange rootChange = (CompositeChange)participant.createChange(new NullProgressMonitor());
		
		Change mainChange = processor.createChange(new NullProgressMonitor());
		mainChange.perform(new NullProgressMonitor());

		checkChanges(rootChange, changeList);
	}

	public static void checkRename(RefactoringProcessor processor, IResource oldObject, String newName, RenameParticipant participant, List<TestChangeStructure> changeList) throws CoreException {
		JobUtils.waitForIdle(2000);

		// Test before renaming
		checkBeforeRefactoring(changeList);

		// Rename
		RenameArguments arguments = new RenameArguments(newName, true);
		boolean initialized = participant.initialize(processor, oldObject, arguments);
		
		assertTrue("Participant has not been initialized", initialized);
		
		RefactoringStatus status = participant.checkConditions(new NullProgressMonitor(), null);
		
		RefactoringStatusEntry[] entries = status.getEntries();
		for(RefactoringStatusEntry entry : entries){
			System.out.println("Refactor status - "+entry.getMessage());
		}
		
		assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));
		
		CompositeChange rootChange = (CompositeChange)participant.createChange(new NullProgressMonitor());
		
		Change mainChange = processor.createChange(new NullProgressMonitor());
		mainChange.perform(new NullProgressMonitor());
		
		checkChanges(rootChange, changeList);
	}
	
	public static void checkBeforeRefactoring(List<TestChangeStructure> changeList){
		for(TestChangeStructure changeStructure : changeList){
			IFile file = changeStructure.getProject().getFile(changeStructure.getFileName());
			String content = null;
			try {
				content = readStream(file);
			} catch (CoreException e) {
				e.printStackTrace();
				fail(e.getMessage());
			}

			for(TestTextChange change : changeStructure.getTextChanges()){
				assertNotSame(change.getText(), content.substring(change.getOffset(), change.getOffset()+change.getLength()));
			}
		}
	}
	
	public static void checkChanges(CompositeChange rootChange, List<TestChangeStructure> changeList) throws CoreException {
		assertNotNull("Root change is null",rootChange);
		
		int numberOfChanges = rootChange.getChildren().length;

		for(int i = 0; i < rootChange.getChildren().length;i++){
			TextFileChange fileChange = (TextFileChange)rootChange.getChildren()[i];

			MultiTextEdit edit = (MultiTextEdit)fileChange.getEdit();
			
			//System.out.println("File - "+fileChange.getFile().getFullPath()+" offset - "+edit.getOffset());
			
			TestChangeStructure change = findChange(changeList, fileChange.getFile());
			if(change != null){
				assertEquals(change.size(), edit.getChildrenSize());
			}
		}

		rootChange.perform(new NullProgressMonitor());
		JobUtils.waitForIdle(2000);

		// Test results
		for(TestChangeStructure changeStructure : changeList){
			IFile file = changeStructure.getProject().getFile(changeStructure.getFileName());
			String content = null;
			content = readStream(file);
			for(TestTextChange change : changeStructure.getTextChanges()){
				assertEquals("There is unexpected change in resource - "+file.getName(),change.getText(), content.substring(change.getOffset(), change.getOffset()+change.getLength()));
			}
		}
		assertEquals("There is unexpected number of changes",changeList.size(), numberOfChanges);
	}
	
	public static TestChangeStructure findChange(List<TestChangeStructure> changeList, IFile file){
		for(TestChangeStructure tcs : changeList){
			if(tcs.getFileName().equals("/"+file.getFullPath().removeFirstSegments(1).toString()))
				return tcs;
		}
		return null;
	}

	
	public static class TestChangeStructure{
		private IProject project;
		private String fileName;
		ArrayList<TestTextChange> textChanges = new ArrayList<TestTextChange>();
		

		public TestChangeStructure(IProject project, String fileName){
			this.project = project;
			this.fileName = fileName;
		}

		public IProject getProject(){
			return project;
		}

		public String getFileName(){
			return fileName;
		}
		
		public ArrayList<TestTextChange> getTextChanges(){
			return textChanges;
		}
		
		public void addTextChange(TestTextChange change){
			textChanges.add(change);
		}
		
		public int size(){
			return textChanges.size();
		}

	}
	
	public static class TestTextChange{
		private int offset;
		private int length;
		private String text;
		
		public TestTextChange(int offset, int length, String text){
			this.offset = offset;
			this.length = length;
			this.text = text;
		}
		
		public int getOffset(){
			return offset;
		}

		public int getLength(){
			return length;
		}

		public String getText(){
			return text;
		}
	}
	
    public static String readStream(InputStream is) {
        StringBuffer sb = new StringBuffer(""); //$NON-NLS-1$
        try {
            byte[] b = new byte[4096];
            while(true) {
                int l = is.read(b, 0, b.length);
                if(l < 0) break;
                sb.append(new String(b, 0, l));
            }
            is.close();
        } catch (IOException e) {
        	e.printStackTrace();
			fail(e.getMessage());
        }
        return sb.toString();
    }
    
    public static String readStream(IFile file) throws CoreException {
		String content = null;
		InputStream in = null;
		try {
			in = file.getContents();
			content = readStream(in);
		} finally {
			if(in!=null) {
				try {
					in.close();
				} catch (IOException e) {
					e.printStackTrace();
					fail(e.getMessage());
				}
			}
		}
		return content;
    }

}
