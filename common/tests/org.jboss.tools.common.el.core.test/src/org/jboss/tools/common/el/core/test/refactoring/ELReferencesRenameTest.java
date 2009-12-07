package org.jboss.tools.common.el.core.test.refactoring;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.MultiTextEdit;
import org.jboss.tools.common.el.ui.refactoring.RenameMethodParticipant;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.test.util.JobUtils;

public class ELReferencesRenameTest extends ELRefactoringTest {
	

	public ELReferencesRenameTest(){
		super("Rename Method Refactoring Test");
	}

	

	public void testRenameMethod() throws CoreException {
		ArrayList<TestChangeStructure> list = new ArrayList<TestChangeStructure>();


		TestChangeStructure structure = new TestChangeStructure(jsfProject, "/WebContent/pages/hello.jsp");
		TestTextChange change = new TestTextChange(353, 4, "name");
		structure.addTextChange(change);
		list.add(structure);

		structure = new TestChangeStructure(jsfProject, "/WebContent/pages/inputUserName.jsp");
		change = new TestTextChange(499, 4, "name");
		structure.addTextChange(change);
		list.add(structure);
		
		IMethod method = getJavaMethod(jsfProject, "demo.User", "getName");

		renameELReferences(method, "alias", list);
	}

	public void testRenameClass() throws CoreException {
		ArrayList<TestChangeStructure> list = new ArrayList<TestChangeStructure>();


		TestChangeStructure structure = new TestChangeStructure(jsfProject, "/WebContent/pages/hello.jsp");
		TestTextChange change = new TestTextChange(349, 4, "user");
		structure.addTextChange(change);
		list.add(structure);

		structure = new TestChangeStructure(jsfProject, "/WebContent/pages/inputUserName.jsp");
		change = new TestTextChange(495, 4, "user");
		structure.addTextChange(change);
		list.add(structure);
		
		IType type = getJavaType(jsfProject, "demo.User");

		renameELReferences(type, "person", list);
	}
	
	private void renameELReferences(IJavaElement element, String newName, List<TestChangeStructure> changeList) throws CoreException{
		JobUtils.waitForIdle();


		// Rename EL references
		RenameMethodParticipant participant = new RenameMethodParticipant();
		participant.initialize(element, newName);
		participant.checkConditions(new NullProgressMonitor(), null);
		CompositeChange rootChange = (CompositeChange)participant.createChange(new NullProgressMonitor());
		
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
		JobUtils.waitForIdle();
		// Test results

		for(TestChangeStructure changeStructure : changeList){
			IFile file = changeStructure.getProject().getFile(changeStructure.getFileName());
			String content = null;
			try {
				content = FileUtil.readStream(file);
			} catch (CoreException e) {
				e.printStackTrace();
				fail(e.getMessage());
			}
			//System.out.println("File - "+file.getName()+" offset - "+changeStructure.getOffset()+" expected - ["+changeStructure.getText()+"] actual - ["+content.substring(changeStructure.getOffset(), changeStructure.getOffset()+changeStructure.getLength())+"]");
			for(TestTextChange change : changeStructure.getTextChanges()){
				assertEquals("There is unexpected change in resource - "+file.getName(), newName, content.substring(change.getOffset(), change.getOffset()+newName.length()));
			}
		}
	}
}