/******************************************************************************* 
 * Copyright (c) 2007-2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *     Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.tests;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.test.util.JUnitUtils;
import org.jboss.tools.test.util.JobUtils;

/**
 * @author eskimo
 *
 */
public class AbstractResourceMarkerTest extends TestCase implements IAnnotationTest {

	public static final String MARKER_TYPE = "org.eclipse.wst.validation.problemmarker";
	
	protected IProject project = null;

	/**
	 * @return the project
	 */
	public IProject getProject() {
		return project;
	}

	/**
	 * @param project the project to set
	 */
	public void setProject(IProject project) {
		this.project = project;
	}

	/**
	 * 
	 */
	public AbstractResourceMarkerTest() {
	}

	/**
	 * 
	 */
	public AbstractResourceMarkerTest(String name) {
		super(name);
	}
	
	protected void copyContentsFile(String originalName, String newContentName) throws CoreException{
		IFile originalFile = project.getFile(originalName);
		IFile newContentFile = project.getFile(newContentName);
		
		copyContentsFile(originalFile, newContentFile);
	}
	
	protected void copyContentsFile(IFile originalFile, String newContentName) throws CoreException{
		IFile newContentFile = project.getFile(newContentName);
		copyContentsFile(originalFile, newContentFile);
	}

	protected void copyContentsFile(IFile originalFile, IFile newContentFile) throws CoreException {
		assertTrue(originalFile.exists());
		assertTrue(newContentFile.exists());
		InputStream is = null;
		try{
			is = newContentFile.getContents();
			originalFile.setContents(is, IFile.FORCE, null);
		} finally {
			if(is!=null) {
				try {
					is.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		originalFile.getProject().build(IncrementalProjectBuilder.INCREMENTAL_BUILD, null);
		JobUtils.waitForIdle();
	}

	public static int findMarkerLine(IResource resource, String type, String errorMessage, boolean pattern)
			throws CoreException {
		int number = -1;
		List<Integer> lines = findMarkerLines(resource, type, errorMessage, pattern);
		if(!lines.isEmpty()) {
			number = lines.get(0);
		}
		return number;
	}

	public static List<Integer> findMarkerLines(IResource resource, String type,
			String pattern) throws CoreException {
		return findMarkerLines(resource, type, pattern, true);
	}

	public static List<Integer> findMarkerLines(IResource resource, String type,
			String errorMessage, boolean pattern) throws CoreException {
		List<Integer> numbers = new ArrayList<Integer>();
		IMarker[] markers = findMarkers(resource, type, errorMessage, pattern);
		for (int i = 0; i < markers.length; i++) {
			numbers.add(markers[i].getAttribute(IMarker.LINE_NUMBER, -1));
		}

		return numbers;
	}

	public static IMarker[] findMarkers(IResource resource, String type, String pattern) throws CoreException {
		return findMarkers(resource, type, pattern, true);
	}

	public static IMarker[] findMarkers(IResource resource, String type, String errorMessage, boolean pattern) throws CoreException {
		List<IMarker> result = new ArrayList<IMarker>();
		IMarker[] markers = resource.findMarkers(type, true, IResource.DEPTH_INFINITE);
		for (int i = 0; i < markers.length; i++) {
			String message = markers[i].getAttribute(IMarker.MESSAGE, ""); //$NON-NLS-1$
			if (pattern?message.matches(errorMessage)||message.equals(errorMessage):message.equals(errorMessage) && markers[i].exists()) {
				result.add(markers[i]);
			}
		}
		return result.toArray(new IMarker[result.size()]);
	}

	public static void assertMarkerIsCreated(IResource resource, MarkerData markerData) throws CoreException {
		assertMarkerIsCreated(resource, markerData.type, markerData.pattern, true, markerData.line);
	}

	public static void assertMarkerIsCreated(IResource resource, String type, String pattern, int... expectedLines) throws CoreException {
		assertMarkerIsCreated(resource, type, pattern, true, expectedLines);
	}

	public static void assertMarkerIsCreated(IResource resource, String type, String errorMessage, boolean pattern, int... expectedLines) 
		throws CoreException {

		List<Integer> lines = findMarkerLines(
				resource, type, errorMessage, pattern);

		StringBuffer expectedString = new StringBuffer();
		StringBuffer realString = new StringBuffer();
		int j = 1;
		for (int line : lines) {
			realString.append(line);
			if(lines.size()>j++) {
				realString.append(", ");
			}
		}
		j = 1;
		for (int expected : expectedLines) {
			expectedString.append(expected);
			if(expectedLines.length>j++) {
				expectedString.append(", ");
			}
		}

		if(lines.isEmpty()) {
			IMarker[] allMarkers = findMarkers(resource, null, ".*", true);
			StringBuffer sb = new StringBuffer("Marker matches the '"); //$NON-NLS-1$
			sb.append(errorMessage).append("' pattern wasn't found. Here is the list of found markers in ").append(resource.getFullPath().toOSString()).append(allMarkers.length==0?" : [": " : [\r\n"); //$NON-NLS-1$ //$NON-NLS-2$
			int i=0;
			for (IMarker marker : allMarkers) {
				String message = marker.getAttribute(IMarker.MESSAGE, ""); //$NON-NLS-1$
				int line = marker.getAttribute(IMarker.LINE_NUMBER, -1);
				String mType = marker.getType();
				sb.append(i).append(") line=\"").append(line).append("\"; type=\"").append(mType).append("\"; message=\"").append(message).append("\";\r\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				i++;
			}
			sb.append("]"); //$NON-NLS-1$
			fail(sb.toString());
		}

		assertEquals("Wrong number of found marker matches the '" + errorMessage + "' pattern. Expected the pattern at the following lines <" + expectedString + "> but was at <" + realString + ">.",  //$NON-NLS-1$//$NON-NLS-2$
				expectedLines.length, lines.size());

		for (int line : lines) {
			boolean found = false;
			for (int expected : expectedLines) {
				if(line==expected) {
					found = true;
					break;
				}
			}
			assertTrue("Marker matches the '" + errorMessage + "' pattern was found at wrong lines. Expected: " + expectedString + " but were: " + realString,  //$NON-NLS-1$//$NON-NLS-2$
					found);
		}
	}

	public static void assertMarkerIsCreatedForGivenPosition(
			IResource resource, String type, String pattern, int lineNumber,
			int startPosition, int endPosition) throws CoreException {
		try {
			//for Windows, where line delimiter is replaced by \r\n
			int lineDelimiterLength = getLineDelimiterLength(resource);
			if(lineDelimiterLength == 2) {
				startPosition += lineNumber - 1;
				endPosition += lineNumber - 1;
			}
		} catch (IOException e) {
			throw new CoreException(new Status(IStatus.ERROR, TestsPlugin.ID, e.getMessage(), e));
		}
		IMarker[] markers = findMarkers(resource, type, pattern, true);
		StringBuffer sb = new StringBuffer("[");
		for (int i = 0; i < markers.length; i++) {
			int line = markers[i].getAttribute(IMarker.LINE_NUMBER, -1);
			int start = markers[i].getAttribute(IMarker.CHAR_START, -1);
			int end = markers[i].getAttribute(IMarker.CHAR_END, -1);
			if(lineNumber==line && start == startPosition && end == endPosition) {
				return;
			}
			if(i>0) {
				sb.append("; ");
			}
			sb.append("line number - ").append(line).append(", start - ").append(start).append(", end - ").append(end);
		}
		sb.append("]");

		fail("Marker matches the '" + pattern + "' pattern wasn't found for line - " + lineNumber + ", start - " + startPosition + ", end - " + endPosition + ". Found markers for given patern: " + sb.toString()); //$NON-NLS-1$ //$NON-NLS-2$
	}

	static int getLineDelimiterLength(IResource resource) throws CoreException, IOException {
		IFile f = (IFile)resource;
		InputStream is = f.getContents();
		byte[] b = new byte[512];
		while(true) {
			int di = is.read(b, 0, b.length);
			if(di < 0) break;
			String s = new String(b, 0, di);
			for (int i = 0; i < s.length(); i++) {
				if(s.indexOf("\r\n") >= 0) return 2;
				if(s.indexOf("\r") >= 0 || s.indexOf("\n") >= 0) return 1;
			}
		}
		return 1;
	}

	public static void assertMarkerIsNotCreated(IResource resource, String type, String pattern) throws CoreException {
		IMarker[] markers = findMarkers(resource, type, pattern);

		assertFalse("Marker matches the '" + pattern + "' pattern was found", markers.length>0); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static void assertMarkerIsNotCreated(IResource resource, String type, String pattern, int expectedLine) throws CoreException {
		assertMarkerIsNotCreated(resource, type, pattern, true, expectedLine);
	}

	public static void assertMarkerIsNotCreated(IResource resource, String type, String errorMessage, boolean pattern, int expectedLine) throws CoreException {
		List<Integer> lines = findMarkerLines(resource, type, errorMessage, pattern);

		assertFalse("Marker matches the '" + errorMessage + "' pattern was found", lines.contains(expectedLine)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static void assertMarkerIsCreated(IResource resource, String type, String pattern) throws CoreException {
		IMarker[] markers = findMarkers(resource, type, pattern);

		assertTrue("Marker matches the '" + pattern + "' pattern wasn't found",  //$NON-NLS-1$ //$NON-NLS-2$
			markers.length>0);
	}

	public static void assertMarkersIsCreated(IResource resource, MarkerData[] markersData) throws CoreException {
		for (MarkerData markerData : markersData) {
			assertMarkerIsCreated(resource, markerData);
		}
	}

	public static int getMarkersNumber(IResource resource) {
		return getMarkersNumber(resource, null);
	}

	public static int getMarkersNumber(IResource resource, IMarkerFilter filter) {
		try{
			IMarker[] markers = resource.findMarkers(null, true, IResource.DEPTH_INFINITE);
			int length = markers.length;
			for(int i=0;i<markers.length;i++){
//				System.out.println("Marker - "+markers[i].getAttribute(IMarker.MESSAGE, ""));  //$NON-NLS-1$//$NON-NLS-2$
				if(markers[i].exists() && (filter==null || !filter.accept(markers[i]))) {
					length--;
				}
			}
			return length;
		}catch(CoreException ex){
			JUnitUtils.fail("Can't get problem markers", ex); //$NON-NLS-1$
		}
		return -1;
	}

	public static String[] getMarkersMessage(IResource resource) {
		return getMarkersMessage(resource, null);
	}

	public static String[] getMarkersMessage(IResource resource, IMarkerFilter filter) {
		List<String> messages = new ArrayList<String>();
		try{
			IMarker[] markers = resource.findMarkers(null, true, IResource.DEPTH_INFINITE);

//			System.out.println("Marker - "+markers[i].getAttribute(IMarker.MESSAGE, ""));  //$NON-NLS-1$//$NON-NLS-2$
			for(int i=0;i<markers.length;i++){
				if(markers[i].exists() && (filter==null || filter.accept(markers[i]))) {
					messages.add(markers[i].getAttribute(IMarker.MESSAGE, "")); //$NON-NLS-1$
				}
			}
		}catch(CoreException ex){
			JUnitUtils.fail("Can't get problem markers", ex); //$NON-NLS-1$
		}
		return messages.toArray(new String[0]);
	}

	public static Integer[] getMarkersNumbersOfLine(IResource resource) {
		return getMarkersNumbersOfLine(resource, null);
	}

	public static Integer[] getMarkersNumbersOfLine(IResource resource, IMarkerFilter filter) {
		List<Integer> numbers = new ArrayList<Integer>();
		try{
			IMarker[] markers = resource.findMarkers(null, true, IResource.DEPTH_INFINITE);

			for(int i=0;i<markers.length;i++){
//				System.out.println("Marker line number - "+markers[i].getAttribute(IMarker.LINE_NUMBER, 0)); //$NON-NLS-1$
				if(markers[i].exists() && (filter==null || filter.accept(markers[i]))) {
					numbers.add(markers[i].getAttribute(IMarker.LINE_NUMBER, 0));
				}
			}
		}catch(CoreException ex){
			JUnitUtils.fail("Can't get problem markers.", ex); //$NON-NLS-1$
		}
		return numbers.toArray(new Integer[0]);
	}

	public static int getMarkersNumberByGroupName(String type, IResource resource, String messageGroup) {
		try{
			IMarker[] markers = resource.findMarkers(type, true, IResource.DEPTH_INFINITE);
			int length = markers.length;
			for (int i = 0; i < markers.length; i++) {
				String groupName = markers[i].getAttribute("groupName", null);
				if(groupName==null || (!groupName.equals(messageGroup) && !groupName.equals("markedKbResource"))) {
					length--;
				}
			}
			return length;
		}catch(CoreException ex){
			JUnitUtils.fail("Can'r get problem markers", ex);
		}
		return -1;
	}

	public static int getMarkersNumberByGroupName(IResource resource, String messageGroup) {
		return getMarkersNumberByGroupName(MARKER_TYPE, resource, messageGroup);
	}

	/**
	 * 
	 * @author eskimo
	 *
	 */
	public static class MarkerData {

		private String type;
		private String pattern;
		private int line = -1;

		public MarkerData(String type, String pattern, int line) {
			this.type = type;
			this.pattern = pattern;
			this.line = line;
		}

		public int getLine() {
			return line;
		}

		public void setLine(int line) {
			this.line = line;
		}

		public String getType() {
			return type;
		}

		public void setType(String type) {
			this.type = type;
		}

		public String getPattern() {
			return pattern;
		}

		public void setPattern(String pattern) {
			this.pattern = pattern;
		}
	}

	public static void assertMarkerIsCreated(IResource resource, String pattern, int... expectedLines) throws CoreException {
		assertMarkerIsCreated(resource, pattern, true, expectedLines);
	}

	public static void assertMarkerIsCreated(IResource resource, String message, boolean pattern, int... expectedLines) throws CoreException {
		assertMarkerIsCreated(resource, AbstractResourceMarkerTest.MARKER_TYPE, pattern?convertMessageToPatern(message):message, pattern, expectedLines);
	}

	public static void assertMarkerIsNotCreated(IResource resource, String message) throws CoreException {
		assertMarkerIsNotCreated(resource, AbstractResourceMarkerTest.MARKER_TYPE, convertMessageToPatern(message));
	}

	public static void assertMarkerIsNotCreated(IResource resource, String message, int expectedLine) throws CoreException {
		assertMarkerIsNotCreated(resource, AbstractResourceMarkerTest.MARKER_TYPE, convertMessageToPatern(message), expectedLine);
	}

	public static void assertMarkerIsCreatedForGivenPosition(IResource resource, String message, int lineNumber, int startPosition, int endPosition) throws CoreException {
		assertMarkerIsCreatedForGivenPosition(resource, AbstractResourceMarkerTest.MARKER_TYPE, convertMessageToPatern(message), lineNumber, startPosition, endPosition);
	}

	public static String convertMessageToPatern(String message) {
		return message.replace("[", "\\[").replace("]", "\\]").replace("<", "\\<").replace(">", "\\>").replace("(", "\\(").replace(")", "\\)")
				.replace("{", "\\{").replace("}", "\\}").replace("'", "\\'");
	}

	@Override
	public void assertAnnotationIsCreated(IResource resource, String pattern, int... expectedLines) throws CoreException {
		assertMarkerIsCreated(resource, pattern, expectedLines);		
	}

	@Override
	public void assertAnnotationIsCreated(IResource resource, String message, boolean pattern, int... expectedLines) throws CoreException {
		assertMarkerIsCreated(resource, message, pattern, expectedLines);
	}

	@Override
	public void assertAnnotationIsNotCreated(IResource resource, String message) throws CoreException {
		assertMarkerIsNotCreated(resource, message);
	}

	@Override
	public void assertAnnotationIsNotCreated(IResource resource, String message, int expectedLine) throws CoreException {
		assertMarkerIsNotCreated(resource, message, expectedLine);
	}

	@Override
	public void assertAnnotationIsCreatedForGivenPosition(IResource resource, String message, int lineNumber, int startPosition, int endPosition) throws CoreException {
		assertMarkerIsCreatedForGivenPosition(resource, message, lineNumber, startPosition, endPosition);
	}
}