package org.jboss.tools.common.base.test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import junit.framework.Assert;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor;
import org.eclipse.jdt.internal.ui.javaeditor.JavaMarkerAnnotation;
import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.internal.Workbench;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.wst.sse.ui.internal.reconcile.ReconcileAnnotationKey;
import org.eclipse.wst.sse.ui.internal.reconcile.TemporaryAnnotation;
import org.eclipse.wst.validation.internal.provisional.core.IMessage;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.editor.ObjectMultiPageEditor;
import org.jboss.tools.common.model.ui.editor.EditorPartWrapper;
import org.jboss.tools.common.quickfix.QuickFixManager;
import org.jboss.tools.common.refactoring.TestableResolutionWithDialog;
import org.jboss.tools.common.refactoring.TestableResolutionWithRefactoringProcessor;
import org.jboss.tools.common.ui.marker.AddSuppressWarningsMarkerResolution;
import org.jboss.tools.common.ui.marker.ConfigureProblemSeverityMarkerResolution;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.common.validation.TempMarkerManager;
import org.jboss.tools.common.validation.ValidationMessage;
import org.jboss.tools.common.validation.java.TempJavaProblem;
import org.jboss.tools.common.validation.java.TempJavaProblemAnnotation;

public class QuickFixTestUtil{
	private static final int MAX_SECONDS_TO_WAIT = 3;
	
	protected ISourceViewer getViewer(IEditorPart editor){
		if(editor instanceof JavaEditor){
			return ((JavaEditor)editor).getViewer();
		}else if(editor instanceof EditorPartWrapper){
			IEditorPart ed = ((EditorPartWrapper)editor).getEditor();
			
			if(ed instanceof ObjectMultiPageEditor){
				((ObjectMultiPageEditor)ed).selectPageByName("Source");
				return ((ObjectMultiPageEditor)ed).getSourceEditor().getTextViewer();
			}else {
				Assert.fail("Editor must be ObjectMultiPageEditor, was - "+ed.getClass());
			}
		}else{
			Assert.fail("editor must be instanceof JavaEditor or EditorPartWrapper, but was "+editor.getClass());
		}
		return null;
	}
	
	protected String getEditorId(String filename) {
		if(filename.endsWith(".xml")){
			return "org.jboss.tools.common.model.ui.editor.EditorPartWrapper";
		}
		IWorkbench workbench = PlatformUI.getWorkbench();
		IEditorRegistry editorRegistry = workbench.getEditorRegistry();
		IEditorDescriptor descriptor = editorRegistry
				.getDefaultEditor(filename);
		if (descriptor != null)
			return descriptor.getId();
		return EditorsUI.DEFAULT_TEXT_EDITOR_ID;
	}

	private void checkForConfigureProblemSeverity(IJavaCompletionProposal[] proposals){
		for(IJavaCompletionProposal proposal : proposals){
			if(proposal.getClass().equals(ConfigureProblemSeverityMarkerResolution.class))
				return;
		}
		Assert.fail("Configure Problem Severity quick fix not found");
	}

	private void checkForAddSuppressWarnings(IFile file, Annotation annotation, IJavaCompletionProposal[] proposals){
		if(annotation instanceof TempJavaProblemAnnotation){
			String severity = ((TempJavaProblemAnnotation)annotation).getMarkerType();
			if(file.getFileExtension().equals("java") && severity.equals(JavaMarkerAnnotation.WARNING_ANNOTATION_TYPE)){
				for(IJavaCompletionProposal proposal : proposals){
					if(proposal.getClass().equals(AddSuppressWarningsMarkerResolution.class))
						return;
				}
				Assert.fail("Add @SuppressWarnings marker resolution not found");
			}
		}
	}
	
	public void checkProposal(IProject project, String fileName, String newFile, String str, int id, Class<? extends IJavaCompletionProposal> proposalClass, boolean checkResult) throws CoreException {
		IFile file = project.getFile(fileName);
		IFile nFile = project.getFile(newFile);

		Assert.assertTrue("File - "+file.getFullPath()+" must be exist",file.exists());
		Assert.assertTrue("File - "+nFile.getFullPath()+" must be exist",nFile.exists());

		IEditorInput input = new FileEditorInput(file);

		IEditorPart editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().openEditor(input,	getEditorId(file.getName()), true);
		ISourceViewer viewer = getViewer(editor);
		
		//MarkerResolutionTestUtil.copyFiles(project, new String[]{fileName});
		
		try{
			// change file
			IDocument document = viewer.getDocument();
			
			String text = FileUtil.getContentFromEditorOrFile(nFile);
			
			document.set(text);
			
			int offset = text.indexOf(str);
			
			Assert.assertTrue("String - "+str+" not found", offset > 0);
			
			int length = str.length();
			Annotation annotation = createAnnotation(file, id, offset, length);
			
			Position position = new Position(offset, length);
			IJavaCompletionProposal[] proposals = getCompletionProposals(annotation, position);
			checkForConfigureProblemSeverity(proposals);
			checkForAddSuppressWarnings(file, annotation, proposals);
			for(IJavaCompletionProposal proposal : proposals){
				if (proposal.getClass().equals(proposalClass)) {
//					if(checkResult){
//						if(proposal instanceof TestableResolutionWithRefactoringProcessor){
//							RefactoringProcessor processor = ((TestableResolutionWithRefactoringProcessor)proposal).getRefactoringProcessor();
//							
//							RefactoringStatus status = processor.checkInitialConditions(new NullProgressMonitor());
//							
//							Assert.assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));
//	
//							status = processor.checkFinalConditions(new NullProgressMonitor(), null);
//	
//							Assert.assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));
//	
//							CompositeChange rootChange = (CompositeChange)processor.createChange(new NullProgressMonitor());
//							
//							rootChange.perform(new NullProgressMonitor());
//						} else if(proposal instanceof TestableResolutionWithDialog){
//							((TestableResolutionWithDialog) proposal).runForTest(null);
//						} else {
//							proposal.apply(document);
//						}
//	
//					}
					return;
				}
			}
			
			Assert.fail("Quick fix: "+proposalClass+" not found");
		}finally{
			Workbench.getInstance().getActiveWorkbenchWindow().getActivePage().closeEditor(editor, false);
//			if(editor.isDirty()){
//				editor.doSave(new NullProgressMonitor());
//			}
			//editor.dispose();
//			MarkerResolutionTestUtil.restoreFiles(project, new String[]{fileName});
		}
	}
	
	public static IJavaCompletionProposal[] getCompletionProposals(Annotation annotation, Position position){
		ArrayList<IJavaCompletionProposal> proposals = new ArrayList<IJavaCompletionProposal>();
		
		if(QuickFixManager.getInstance().hasProposals(annotation, position)){
			List<IJavaCompletionProposal> list = QuickFixManager.getInstance().getProposals(annotation, position);
			proposals.addAll(list);
		}
		
		return proposals.toArray(new IJavaCompletionProposal[]{});
	}

	private static void checkResults(IFile file, String text) throws CoreException{
		String fileContent = FileUtil.readStream(file);
		
		Assert.assertEquals("Wrong result of resolution", fileContent, text);
	}
	
	protected Annotation createAnnotation(IFile file, int quickFixId, int offset, int length){
		if("java".equals(file.getFileExtension())){
			ICompilationUnit compilationUnit = EclipseUtil.getCompilationUnit(file);
			
			synchronized(compilationUnit) {
				try {
					compilationUnit.reconcile(ICompilationUnit.NO_AST, true, null, null);
				} catch (JavaModelException e) {
					Assert.fail(e.getMessage());
				}
			}
			Assert.assertNotNull("Compilation unit not found for file - "+file.getName());
			
			ValidationMessage message = new ValidationMessage(IMessage.NORMAL_SEVERITY, "Message", compilationUnit.getResource());
			message.setOffset(offset);
			message.setLength(length);
			message.setLineNo(1);
			message.setBundleName("org.jboss.tools.cdi.internal.core.validation.messages");
			message.setAttribute(TempMarkerManager.AS_YOU_TYPE_VALIDATION_ANNOTATION_ATTRIBUTE, Boolean.TRUE);
			message.setAttribute(TempMarkerManager.MESSAGE_ID_ATTRIBUTE_NAME, quickFixId);
			message.setAttribute(TempMarkerManager.PREFERENCE_KEY_ATTRIBUTE_NAME, "preferenceKey");
			message.setAttribute(TempMarkerManager.PREFERENCE_PAGE_ID_NAME, "pref_page_id");
			message.setAttribute(TempMarkerManager.MESSAGE_TYPE_ATTRIBUTE_NAME, JavaMarkerAnnotation.WARNING_ANNOTATION_TYPE);
			
			TempJavaProblem problem = new TempJavaProblem(message, "origin");
			
			TempJavaProblemAnnotation annotation = new TempJavaProblemAnnotation(problem, compilationUnit);
			
			return annotation;
		}else{
			TemporaryAnnotation annotation = new TemporaryAnnotation(new Position(offset, length), TemporaryAnnotation.ANNOT_WARNING, "message", null, quickFixId);
			Map attributes = new HashMap();
			attributes.put(TempMarkerManager.AS_YOU_TYPE_VALIDATION_ANNOTATION_ATTRIBUTE, Boolean.TRUE);
			attributes.put(TempMarkerManager.MESSAGE_ID_ATTRIBUTE_NAME, quickFixId);
			attributes.put(TempMarkerManager.PREFERENCE_KEY_ATTRIBUTE_NAME, "preferenceKey");
			attributes.put(TempMarkerManager.PREFERENCE_PAGE_ID_NAME, "pref_page_id");
			attributes.put(TempMarkerManager.MESSAGE_TYPE_ATTRIBUTE_NAME, JavaMarkerAnnotation.WARNING_ANNOTATION_TYPE);
			annotation.setAttributes(attributes);
			
			return annotation;
		}
	}
}
