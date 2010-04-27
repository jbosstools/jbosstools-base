package org.jboss.tools.tests;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.RenameProcessor;
import org.eclipse.text.edits.MultiTextEdit;
import org.jboss.tools.test.util.JobUtils;

public class AbstractRefactorTest extends TestCase{
	
	public AbstractRefactorTest(String name){
		super(name);
	}
	
	protected void checkRename(RenameProcessor processor, List<TestChangeStructure> changeList) throws CoreException{
		JobUtils.waitForIdle(2000);

		// Test before renaming
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

		// Rename
		processor.checkInitialConditions(new NullProgressMonitor());
		processor.checkFinalConditions(new NullProgressMonitor(), null);
		CompositeChange rootChange = (CompositeChange)processor.createChange(new NullProgressMonitor());
		
		assertEquals("There is unexpected number of changes",changeList.size(), rootChange.getChildren().length);

		for(int i = 0; i < rootChange.getChildren().length;i++){
			TextFileChange fileChange = (TextFileChange)rootChange.getChildren()[i];

			MultiTextEdit edit = (MultiTextEdit)fileChange.getEdit();
			
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
	}

	
	protected TestChangeStructure findChange(List<TestChangeStructure> changeList, IFile file){
		for(TestChangeStructure tcs : changeList){
			if(tcs.getFileName().equals("/"+file.getFullPath().removeFirstSegments(1).toString()))
				return tcs;
		}
		return null;
	}

	
	public class TestChangeStructure{
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
	
	public class TestTextChange{
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
