package org.jboss.tools.common.base.test;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import junit.framework.Assert;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor;
import org.eclipse.jdt.internal.ui.javaeditor.JavaMarkerAnnotation;
import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.jboss.tools.common.base.test.validation.TestUtil;
import org.jboss.tools.common.quickfix.QuickFixManager;
import org.jboss.tools.common.refactoring.TestableResolutionWithDialog;
import org.jboss.tools.common.refactoring.TestableResolutionWithRefactoringProcessor;
import org.jboss.tools.common.ui.marker.AddSuppressWarningsMarkerResolution;
import org.jboss.tools.common.ui.marker.ConfigureProblemSeverityMarkerResolution;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.common.validation.java.TempJavaProblemAnnotation;
import org.jboss.tools.test.util.JobUtils;

public class QuickFixTestUtil{
	private static final int MAX_SECONDS_TO_WAIT = 10;
	
	protected ISourceViewer getViewer(IEditorPart editor){
		if(editor instanceof JavaEditor){
			return ((JavaEditor)editor).getViewer();
		}
		return null;
	}
	
	protected String getEditorID(){
		return "org.eclipse.jdt.ui.CompilationUnitEditor";
	}

	private void checkForConfigureProblemSeverity(IJavaCompletionProposal[] proposals){
		for(IJavaCompletionProposal proposal : proposals){
			if(proposal.getClass().equals(ConfigureProblemSeverityMarkerResolution.class))
				return;
		}
		Assert.fail("Configure Problem Severity quick fix not found");
	}

	private void checkForAddSuppressWarnings(IFile file, TempJavaProblemAnnotation annotation, IJavaCompletionProposal[] proposals){
		String severity = annotation.getMarkerType();
		if(file.getFileExtension().equals("java") && severity.equals(JavaMarkerAnnotation.WARNING_ANNOTATION_TYPE)){
			for(IJavaCompletionProposal proposal : proposals){
				if(proposal.getClass().equals(AddSuppressWarningsMarkerResolution.class))
					return;
			}
			Assert.fail("Add @SuppressWarnings marker resolution not found");
		}
	}
	
	public void checkPrpposal(IProject project, String fileName, String newFile, String idName, int id, Class<? extends IJavaCompletionProposal> proposalClass) throws CoreException {
		IFile file = project.getFile(fileName);
		IFile nFile = project.getFile(newFile);

		Assert.assertTrue("File - "+file.getFullPath()+" must be exist",file.exists());
		Assert.assertTrue("File - "+nFile.getFullPath()+" must be exist",nFile.exists());

		IEditorInput input = new FileEditorInput(file);

		IEditorPart editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().openEditor(input,	getEditorID(), true);
		ISourceViewer viewer = getViewer(editor);
		
		// change file
		IDocument document = viewer.getDocument();
		
		String text = FileUtil.getContentFromEditorOrFile(nFile);
		
		document.set(text);
		
		// Find annotation
		TempJavaProblemAnnotation[] annotations = waitForProblemAnnotationAppearance(viewer);
		
		for(TempJavaProblemAnnotation annotation : annotations){
			IJavaCompletionProposal[] proposals = getCompletionProposals(annotation);
			checkForConfigureProblemSeverity(proposals);
			checkForAddSuppressWarnings(file, annotation, proposals);
			for(IJavaCompletionProposal proposal : proposals){
				if (proposal.getClass().equals(proposalClass)) {

					if(proposal instanceof TestableResolutionWithRefactoringProcessor){
						RefactoringProcessor processor = ((TestableResolutionWithRefactoringProcessor)proposal).getRefactoringProcessor();
						
						RefactoringStatus status = processor.checkInitialConditions(new NullProgressMonitor());
						
						Assert.assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));

						status = processor.checkFinalConditions(new NullProgressMonitor(), null);

						Assert.assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));

						CompositeChange rootChange = (CompositeChange)processor.createChange(new NullProgressMonitor());
						
						rootChange.perform(new NullProgressMonitor());
					} else if(proposal instanceof TestableResolutionWithDialog){
						((TestableResolutionWithDialog) proposal).runForTest(null);
					} else {
						proposal.apply(document);
					}

					TestUtil.validate(file);

					TempJavaProblemAnnotation[] newAnnotations = waitForProblemAnnotationAppearance(viewer);

					Assert.assertTrue("Quick fix did not decrease number of problems. was: "+annotations.length+" now: "+newAnnotations.length, newAnnotations.length < annotations.length);

					//checkResults(project, fileNames, results);

					return;
				}
			}
		}
		
		Assert.fail("Quick fix: "+proposalClass+" not found");
	}
	
	public static IJavaCompletionProposal[] getCompletionProposals(TempJavaProblemAnnotation annotation){
		ArrayList<IJavaCompletionProposal> proposals = new ArrayList<IJavaCompletionProposal>();
		
		if(QuickFixManager.getInstance().hasProposals(annotation)){
			List<IJavaCompletionProposal> list = QuickFixManager.getInstance().getProposals(annotation);
			proposals.addAll(list);
		}
		
		return proposals.toArray(new IJavaCompletionProposal[]{});
	}

	private static void checkResults(IProject project, String[] fileNames, String[] results) throws CoreException{
		for(int i = 0; i < results.length; i++){
			IFile file = project.getFile(fileNames[i]);
			IFile resultFile = project.getFile(results[i]);

			String fileContent = FileUtil.readStream(file);
			String resultContent = FileUtil.readStream(resultFile);
			
			Assert.assertEquals("Wrong result of resolution", resultContent, fileContent);
		}
	}
	
	protected TempJavaProblemAnnotation[] waitForProblemAnnotationAppearance(final ISourceViewer viewer) {
		final ArrayList<TempJavaProblemAnnotation> annotations = new ArrayList<TempJavaProblemAnnotation>();

		Display.getDefault().syncExec(new Runnable() {
			public void run() {
				int secondsLeft = MAX_SECONDS_TO_WAIT;
				boolean isFirstPass = true;
				while (secondsLeft-- > 0) {
					if (!isFirstPass) {
						JobUtils.delay(1000);

						// clean differed events
						while (Display.getCurrent().readAndDispatch())
							;
					} else {
						secondsLeft++; // because the wait step was skipped
					}

					//boolean found = false;
					IAnnotationModel annotationModel = viewer.getAnnotationModel();
					Iterator it = annotationModel.getAnnotationIterator();
					while (it.hasNext()) {
						Object o = it.next();

						if (o instanceof TempJavaProblemAnnotation){
							annotations.add((TempJavaProblemAnnotation) o);
						}

					}
					isFirstPass = false;
				}
			}
		});

		return annotations.toArray(new TempJavaProblemAnnotation[]{});
	}
}
