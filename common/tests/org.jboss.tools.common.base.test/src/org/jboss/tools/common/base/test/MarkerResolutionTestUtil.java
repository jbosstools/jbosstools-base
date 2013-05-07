package org.jboss.tools.common.base.test;

import java.io.IOException;
import java.io.InputStream;

import junit.framework.Assert;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.ide.IDE;
import org.jboss.tools.common.base.test.validation.TestUtil;
import org.jboss.tools.common.refactoring.TestableResolutionWithDialog;
import org.jboss.tools.common.refactoring.TestableResolutionWithRefactoringProcessor;
import org.jboss.tools.common.ui.marker.AddSuppressWarningsMarkerResolution;
import org.jboss.tools.common.ui.marker.ConfigureProblemSeverityMarkerResolution;
import org.jboss.tools.common.util.FileUtil;

public class MarkerResolutionTestUtil{

	private static void checkForConfigureProblemSeverity(IMarkerResolution[] resolutions){
		for(IMarkerResolution resolution : resolutions){
			if(resolution.getClass().equals(ConfigureProblemSeverityMarkerResolution.class))
				return;
		}
		Assert.fail("Configure Problem Severity marker resolution not found");
	}

	private static void checkForAddSuppressWarnings(IFile file, IMarker marker, IMarkerResolution[] resolutions){
		int severity = marker.getAttribute(IMarker.SEVERITY, 0);
		if(file.getFileExtension().equals("java") && severity == IMarker.SEVERITY_WARNING){
			for(IMarkerResolution resolution : resolutions){
				if(resolution.getClass().equals(AddSuppressWarningsMarkerResolution.class))
					return;
			}
			Assert.fail("Add @SuppressWarnings marker resolution not found");
		}
	}
	
	public static void checkResolution(IProject project, String[] fileNames, String markerType, String idName, int id, Class<? extends IMarkerResolution> resolutionClass) throws CoreException {
		checkResolution(project, fileNames, new String[]{}, markerType, idName, id, resolutionClass);
	}
	
	public static void checkResolution(IProject project, String[] fileNames, String[] results, String markerType, String idName, int id, Class<? extends IMarkerResolution> resolutionClass) throws CoreException {
		IFile file = project.getFile(fileNames[0]);

		Assert.assertTrue("File - "+file.getFullPath()+" must be exist",file.exists());

		copyFiles(project, fileNames);
		TestUtil.validate(file);

		try{
			file = project.getFile(fileNames[0]);
			IMarker[] markers = file.findMarkers(markerType, true,	IResource.DEPTH_INFINITE);

			for (int i = 0; i < markers.length; i++) {
				IMarker marker = markers[i];
				Integer attribute = ((Integer) marker
						.getAttribute(idName));
				if (attribute != null){
					int messageId = attribute.intValue();
					if(messageId == id){
						IMarkerResolution[] resolutions = IDE.getMarkerHelpRegistry()
								.getResolutions(marker);
						checkForConfigureProblemSeverity(resolutions);
						checkForAddSuppressWarnings(file, marker, resolutions);
						for (int j = 0; j < resolutions.length; j++) {
							IMarkerResolution resolution = resolutions[j];
							if (resolution.getClass().equals(resolutionClass)) {

								if(resolution instanceof TestableResolutionWithRefactoringProcessor){
									RefactoringProcessor processor = ((TestableResolutionWithRefactoringProcessor)resolution).getRefactoringProcessor();
									
									RefactoringStatus status = processor.checkInitialConditions(new NullProgressMonitor());
									
//									RefactoringStatusEntry[] entries = status.getEntries();
//									for(RefactoringStatusEntry entry : entries){
//										System.out.println("Refactor status - "+entry.getMessage());
//									}

									Assert.assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));

									status = processor.checkFinalConditions(new NullProgressMonitor(), null);

//									entries = status.getEntries();
//									for(RefactoringStatusEntry entry : entries){
//										System.out.println("Refactor status - "+entry.getMessage());
//									}

									Assert.assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));

									CompositeChange rootChange = (CompositeChange)processor.createChange(new NullProgressMonitor());
									
//									for(Change fileChange : rootChange.getChildren()){
//										if(fileChange instanceof JBDSFileChange){
//											((JBDSFileChange)fileChange).setSaveMode(TextFileChange.FORCE_SAVE);
//										}
//									}
									
									rootChange.perform(new NullProgressMonitor());
								} else if(resolution instanceof TestableResolutionWithDialog){
									((TestableResolutionWithDialog) resolution).runForTest(marker);
								} else {
									resolution.run(marker);
								}

								TestUtil.validate(file);

								file = project.getFile(fileNames[0]);
								IMarker[] newMarkers = file.findMarkers(markerType, true,	IResource.DEPTH_INFINITE);

								Assert.assertTrue("Marker resolution did not decrease number of problems. was: "+markers.length+" now: "+newMarkers.length, newMarkers.length < markers.length);

								checkResults(project, fileNames, results);

								return;
							}
						}
						Assert.fail("Marker resolution: "+resolutionClass+" not found");
					}
				}
			}
			Assert.fail("Problem marker with id: "+id+" not found");
		}finally{
			restoreFiles(project, fileNames);
			TestUtil.validate(file);
		}
	}
	
	public static void checkResolution(IProject project, String markerType, String idName, int id, Class<? extends IMarkerResolution> resolutionClass) throws CoreException {
		TestUtil.validate(project);

		try{
			IMarker[] markers = project.findMarkers(markerType, true,	IResource.DEPTH_INFINITE);

			for (int i = 0; i < markers.length; i++) {
				IMarker marker = markers[i];
				Integer attribute = ((Integer) marker
						.getAttribute(idName));
				if (attribute != null){
					int messageId = attribute.intValue();
					if(messageId == id){
						IMarkerResolution[] resolutions = IDE.getMarkerHelpRegistry()
								.getResolutions(marker);
						checkForConfigureProblemSeverity(resolutions);
						for (int j = 0; j < resolutions.length; j++) {
							IMarkerResolution resolution = resolutions[j];
							if (resolution.getClass().equals(resolutionClass)) {

								if(resolution instanceof TestableResolutionWithRefactoringProcessor){
									RefactoringProcessor processor = ((TestableResolutionWithRefactoringProcessor)resolution).getRefactoringProcessor();
									
									RefactoringStatus status = processor.checkInitialConditions(new NullProgressMonitor());
									
									Assert.assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));

									status = processor.checkFinalConditions(new NullProgressMonitor(), null);

									Assert.assertNull("Rename processor returns fatal error", status.getEntryMatchingSeverity(RefactoringStatus.FATAL));

									CompositeChange rootChange = (CompositeChange)processor.createChange(new NullProgressMonitor());
									
									rootChange.perform(new NullProgressMonitor());
								} else if(resolution instanceof TestableResolutionWithDialog){
									((TestableResolutionWithDialog) resolution).runForTest(marker);
								} else {
									resolution.run(marker);
								}

								TestUtil._waitForValidation(project);

								IMarker[] newMarkers = project.findMarkers(markerType, true,	IResource.DEPTH_INFINITE);

								Assert.assertTrue("Marker resolution did not decrease number of problems. was: "+markers.length+" now: "+newMarkers.length, newMarkers.length < markers.length);

								return;
							}
						}
						Assert.fail("Marker resolution: "+resolutionClass+" not found");
					}
				}
			}
			Assert.fail("Problem marker with id: "+id+" not found");
		}finally{
			TestUtil.validate(project);
		}
	}

	public static void copyFiles(IProject project, String[] fileNames) throws CoreException{
		for(String fileName : fileNames){
			IFile file = project.getFile(fileName);
			IFile copyFile = project.getFile(fileName+".copy");

			if(copyFile.exists())
				copyFile.delete(true, null);

			InputStream is = null;
			try{
				is = file.getContents();
				copyFile.create(is, true, null);
			} finally {
				if(is!=null) {
					try {
						is.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
		}
	}

	public static void restoreFiles(IProject project, String[] fileNames) throws CoreException {
		for(String fileName : fileNames){
			IFile file = project.getFile(fileName);
			IFile copyFile = project.getFile(fileName+".copy");
			InputStream is = null;
			try{
				is = copyFile.getContents();
				file.setContents(is, true, false, null);
			} finally {
				if(is!=null) {
					try {
						is.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
			copyFile.delete(true, null);
		}
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

}
