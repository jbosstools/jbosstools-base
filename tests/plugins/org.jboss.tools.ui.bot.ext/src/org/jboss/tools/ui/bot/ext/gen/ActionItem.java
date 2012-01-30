/**
* Generated Thu Apr 08 16:50:59 CEST 2010 JBDS 3.0.0-#50-GA
*/
package org.jboss.tools.ui.bot.ext.gen;

import java.util.List;
import java.util.Vector;

public class ActionItem {

/**
* gets string representation of item
*/
public static String getItemString(IActionItem item) {

	StringBuilder sb = new StringBuilder();
	for (String s :item.getGroupPath()) {
		sb.append(s+"->");
	}
	sb.append(item.getName());
	return sb.toString();
}
	/**
	 * creates new action item instance from given path 
	 * @param path
	 * @return
	 */
	public static IActionItem create(final String...path) {
		if (path.length<1) {
			throw new IllegalArgumentException("path must contain at least 1 item");
		}
		return new IActionItem() {
	
			@Override
			public String getName() {
				// TODO Auto-generated method stub
				return path[path.length-1];
			}
	
			@Override
			public List<String> getGroupPath() {
				List<String> l = new Vector<String>();
				for (int i=0; i<path.length-1;i++) {
					l.add(path[i]);
				}
				return l;
			}
			
		};
	}
	public static class View {
		/**
		 * creates new action item instance from given path 
		 * @param path
		 * @return
		 */
		public static IView create(final String...path) {
			if (path.length<1) {
				throw new IllegalArgumentException("path must contain at least 1 item");
			}
			return new IView() {
		
				@Override
				public String getName() {
					// TODO Auto-generated method stub
					return path[path.length-1];
				}
		
				@Override
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					for (int i=0; i<path.length-1;i++) {
						l.add(path[i]);
					}
					return l;
				}
				
			};
		}
		public static class JavaBrowsingTypes {
			/**
			* represents item : Java Browsing->Types
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Types";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java Browsing");
					return l;
				}
			};
			}
		public static class GeneralSearch {
			/**
			* represents item : General->Search
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Search";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class GeneralProblems {
			/**
			* represents item : General->Problems
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Problems";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class DebugExpressions {
			/**
			* represents item : Debug->Expressions
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Expressions";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Debug");
					return l;
				}
			};
			}
		public static class ServerServers {
			/**
			* represents item : Server->Servers
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Servers";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Server");
					return l;
				}
			};
			}
		
		public static class JAXWSAnnotationProperties {
			/**
			* represents item : JAX-WS->Annotation Properties
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Annotation Properties";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JAX-WS");
					return l;
				}
			};
			}
		
		public static class PluginDevelopmentPluginDependencies {
			/**
			* represents item : Plug-in Development->Plug-in Dependencies
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Plug-in Dependencies";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			}
		public static class HelpCheatSheets {
			/**
			* represents item : Help->Cheat Sheets
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Cheat Sheets";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Help");
					return l;
				}
			};
			}
		public static class HibernateHibernateQueryResult {
			/**
			* represents item : Hibernate->Hibernate Query Result
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Hibernate Query Result";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Hibernate");
					return l;
				}
			};
			}
		public static class GeneralTasks {
			/**
			* represents item : General->Tasks
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Tasks";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class GeneralClassicSearch {
			/**
			* represents item : General->Classic Search
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Classic Search";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JBossToolsWebJBossToolsPalette {
			/**
			* represents item : JBoss Tools Web->JBoss Tools Palette
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "JBoss Tools Palette";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			}
		public static class DroolsProcessInstance {
			/**
			* represents item : Drools->Process Instance
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Process Instance";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			}
		public static class XMLStylesheet {
			/**
			* represents item : XML->Stylesheet
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Stylesheet";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			}
		public static class JavaBrowsingProjects {
			/**
			* represents item : Java Browsing->Projects
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Projects";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java Browsing");
					return l;
				}
			};
			}
		public static class DebugBreakpoints {
			/**
			* represents item : Debug->Breakpoints
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Breakpoints";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Debug");
					return l;
				}
			};
			}
		public static class SpringAOPEventTrace {
			/**
			* represents item : Spring->AOP Event Trace
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "AOP Event Trace";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					return l;
				}
			};
			}
		public static class DebugMemory {
			/**
			* represents item : Debug->Memory
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Memory";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Debug");
					return l;
				}
			};
			}
		public static class JavaJavadoc {
			/**
			* represents item : Java->Javadoc
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Javadoc";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class JMXMBeanExplorer {
			/**
			* represents item : JMX->MBean Explorer
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "MBean Explorer";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JMX");
					return l;
				}
			};
			}
		public static class GeneralInternalWebBrowser {
			/**
			* represents item : General->Internal Web Browser
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Internal Web Browser";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class SVNSVNRepositoryBrowser {
			/**
			* represents item : SVN->SVN Repository Browser 
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "SVN Repository Browser ";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("SVN");
					return l;
				}
			};
			}
		public static class DataManagementDataSourceExplorer {
			/**
			* represents item : Data Management->Data Source Explorer
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Data Source Explorer";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					return l;
				}
			};
			}
		public static class GeneralErrorLog {
			/**
			* represents item : General->Error Log
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Error Log";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JavaJUnit {
			/**
			* represents item : Java->JUnit
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "JUnit";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class JavaHierarchy {
			/**
			* represents item : Java->Hierarchy
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Hierarchy";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class DroolsProcessInstances {
			/**
			* represents item : Drools->Process Instances
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Process Instances";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			}
		public static class XMLResult {
			/**
			* represents item : XML->Result
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Result";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			}
		public static class DroolsGlobalData {
			/**
			* represents item : Drools->Global Data
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Global Data";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			}
		public static class JavaTestNG {
			/**
			* represents item : Java->TestNG
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "TestNG";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class DataManagementSQLResults {
			/**
			* represents item : Data Management->SQL Results
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "SQL Results";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					return l;
				}
			};
			}
		public static class PluginDevelopmentPluginRegistry {
			/**
			* represents item : Plug-in Development->Plug-in Registry
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Plug-in Registry";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			}
		public static class GeneralPalette {
			/**
			* represents item : General->Palette
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Palette";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class SVNRevisionProperties {
			/**
			* represents item : SVN->Revision Properties
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Revision Properties";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("SVN");
					return l;
				}
			};
			}
		public static class DebugDisplay {
			/**
			* represents item : Debug->Display
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Display";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Debug");
					return l;
				}
			};
			}
		public static class DroolsWorkingMemory {
			/**
			* represents item : Drools->Working Memory
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Working Memory";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			}
		public static class JBossToolsWebCSSProperties {
			/**
			* represents item : JBoss Tools Web->CSS Properties
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "CSS Properties";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			}
		public static class SeamSeamComponents {
			/**
			* represents item : Seam->Seam Components
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Seam Components";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Seam");
					return l;
				}
			};
			}
		public static class JavaDeclaration {
			/**
			* represents item : Java->Declaration
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Declaration";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class GuvnorGuvnorRepositories {
			/**
			* represents item : Guvnor->Guvnor Repositories
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Guvnor Repositories";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Guvnor");
					return l;
				}
			};
			}
		public static class SpringBeansCrossReferences {
			/**
			* represents item : Spring->Beans Cross References
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Beans Cross References";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					return l;
				}
			};
			}
		public static class DroolsRules {
			/**
			* represents item : Drools->Rules
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Rules";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			}
		public static class ServerServerLog {
			/**
			* represents item : Server->Server Log
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Server Log";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Server");
					return l;
				}
			};
			}
		public static class SVNSVNLocks {
			/**
			* represents item : SVN->SVN Locks
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "SVN Locks";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("SVN");
					return l;
				}
			};
			}
		public static class JavaServerFacesJSFComponentTree {
			/**
			* represents item : JavaServer Faces->JSF Component Tree
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "JSF Component Tree";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JavaServer Faces");
					return l;
				}
			};
			}
		public static class CVSCVSRepositories {
			/**
			* represents item : CVS->CVS Repositories
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "CVS Repositories";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("CVS");
					return l;
				}
			};
			}
		public static class SWTBotCategoryEclipseSpyView {
			/**
			* represents item : SWTBot Category->EclipseSpy View
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "EclipseSpy View";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("SWTBot Category");
					return l;
				}
			};
			}
		public static class DroolsAgenda {
			/**
			* represents item : Drools->Agenda
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Agenda";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			}
		public static class GeneralNavigator {
			/**
			* represents item : General->Navigator
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Navigator";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class GeneralOutline {
			/**
			* represents item : General->Outline
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Outline";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JBossToolsWebWebProjects {
			/**
			* represents item : JBoss Tools Web->Web Projects
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Web Projects";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			}
		public static class CVSCVSEditors {
			/**
			* represents item : CVS->CVS Editors
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "CVS Editors";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("CVS");
					return l;
				}
			};
			}
		public static class HibernateQueryParameters {
			/**
			* represents item : Hibernate->Query Parameters
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Query Parameters";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Hibernate");
					return l;
				}
			};
			}
		public static class DebugVariables {
			/**
			* represents item : Debug->Variables
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Variables";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Debug");
					return l;
				}
			};
			}
		public static class JPAJPADetails {
			/**
			* represents item : JPA->JPA Details
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "JPA Details";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JPA");
					return l;
				}
			};
			}
		public static class JavaPackageExplorer {
			/**
			* represents item : Java->Package Explorer
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Package Explorer";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class HibernateHibernateDynamicSQLPreview {
			/**
			* represents item : Hibernate->Hibernate Dynamic SQL Preview
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Hibernate Dynamic SQL Preview";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Hibernate");
					return l;
				}
			};
			}
		public static class AntAnt {
			/**
			* represents item : Ant->Ant
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Ant";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Ant");
					return l;
				}
			};
			}
		public static class JavaBrowsingMembers {
			/**
			* represents item : Java Browsing->Members
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Members";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java Browsing");
					return l;
				}
			};
			}
		public static class JavaServerFacesTagRegistry {
			/**
			* represents item : JavaServer Faces->Tag Registry
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Tag Registry";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JavaServer Faces");
					return l;
				}
			};
			}
		public static class APIToolingAPITooling {
			/**
			* represents item : API Tooling->API Tooling
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "API Tooling";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("API Tooling");
					return l;
				}
			};
			}
		public static class GeneralConsole {
			/**
			* represents item : General->Console
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Console";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class GeneralProperties {
			/**
			* represents item : General->Properties
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Properties";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class HelpHelp {
			/**
			* represents item : Help->Help
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Help";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Help");
					return l;
				}
			};
			}
		public static class SVNSVNRepositories {
			/**
			* represents item : SVN->SVN Repositories
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "SVN Repositories";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("SVN");
					return l;
				}
			};
			}
		public static class GeneralSnippets {
			/**
			* represents item : General->Snippets
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Snippets";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class TeamHistory {
			/**
			* represents item : Team->History
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "History";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					return l;
				}
			};
			}
		public static class GeneralBookmarks {
			/**
			* represents item : General->Bookmarks
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Bookmarks";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class GuvnorGuvnorResourceHistory {
			/**
			* represents item : Guvnor->Guvnor Resource History
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Guvnor Resource History";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Guvnor");
					return l;
				}
			};
			}
		public static class HibernateHibernateConfigurations {
			/**
			* represents item : Hibernate->Hibernate Configurations
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Hibernate Configurations";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Hibernate");
					return l;
				}
			};
			}
		public static class JavaScriptJSDoc {
			/**
			* represents item : JavaScript->JSDoc
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "JSDoc";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JavaScript");
					return l;
				}
			};
			}
		public static class ServerJBossServerViewDeprecated {
			/**
			* represents item : Server->JBoss Server View (Deprecated)
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "JBoss Server View (Deprecated)";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Server");
					return l;
				}
			};
			}
		public static class SpringSpringExplorer {
			/**
			* represents item : Spring->Spring Explorer
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Spring Explorer";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					return l;
				}
			};
			}
		public static class GeneralMarkers {
			/**
			* represents item : General->Markers
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Markers";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JavaCallHierarchy {
			/**
			* represents item : Java->Call Hierarchy
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Call Hierarchy";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class JBossjBPMOverviewjBPM3 {
			/**
			* represents item : JBoss jBPM->Overview (jBPM 3)
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Overview (jBPM 3)";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss jBPM");
					return l;
				}
			};
			}
		public static class PluginDevelopmentTargetPlatformState {
			/**
			* represents item : Plug-in Development->Target Platform State
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Target Platform State";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			}
		public static class SVNSVNProperties {
			/**
			* represents item : SVN->SVN Properties
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "SVN Properties";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("SVN");
					return l;
				}
			};
			}
		public static class JavaBrowsingPackages {
			/**
			* represents item : Java Browsing->Packages
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Packages";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java Browsing");
					return l;
				}
			};
			}
		public static class JPAJPAStructure {
			/**
			* represents item : JPA->JPA Structure
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "JPA Structure";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JPA");
					return l;
				}
			};
			}
		public static class DebugModules {
			/**
			* represents item : Debug->Modules
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Modules";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Debug");
					return l;
				}
			};
			}
		public static class TeamSynchronize {
			/**
			* represents item : Team->Synchronize
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Synchronize";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					return l;
				}
			};
			}
		public static class JavaScriptScriptExplorer {
			/**
			* represents item : JavaScript->Script Explorer
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Script Explorer";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JavaScript");
					return l;
				}
			};
			}
		public static class DataManagementExecutionPlan {
			/**
			* represents item : Data Management->Execution Plan
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Execution Plan";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					return l;
				}
			};
			}
		public static class GeneralTemplates {
			/**
			* represents item : General->Templates
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Templates";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class DebugDebug {
			/**
			* represents item : Debug->Debug
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Debug";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Debug");
					return l;
				}
			};
			}
		public static class DebugRegisters {
			/**
			* represents item : Debug->Registers
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Registers";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Debug");
					return l;
				}
			};
			}
		public static class GeneralProgress {
			/**
			* represents item : General->Progress
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Progress";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JBossToolsProjectarchives {
			/**
			* represents item : JBoss Tools->Project archives
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Project archives";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					return l;
				}
			};
			}
		public static class DebugTCPIPMonitor {
			/**
			* represents item : Debug->TCP/IP Monitor
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "TCP/IP Monitor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Debug");
					return l;
				}
			};
			}
		public static class DroolsAudit {
			/**
			* represents item : Drools->Audit
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Audit";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			}
		public static class GeneralProjectExplorer {
			/**
			* represents item : General->Project Explorer
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Project Explorer";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JBossToolsWebCSSPreview {
			/**
			* represents item : JBoss Tools Web->CSS Preview
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "CSS Preview";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			}
		public static class XMLXPath {
			/**
			* represents item : XML->XPath
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "XPath";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			}
		public static class PluginDevelopmentPlugins {
			/**
			* represents item : Plug-in Development->Plug-ins
			*/
			public static final IView LABEL = new IView() {
				public String getName() { return "Plug-ins";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			}

		}

	public static class Server {
		public static class ApacheTomcatv55Server {
			/**
			* represents item : Apache->Tomcat v5.5 Server
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "Tomcat v5.5 Server";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class ObjectWebJOnASv4 {
			/**
			* represents item : ObjectWeb->JOnAS v4
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JOnAS v4";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("ObjectWeb");
					return l;
				}
			};
			/**
			* text field labeled 'JonAS Configuration Directory:'
			*/
			public static final String TEXT_JONAS_CONFIGURATION_DIRECTORY = "JonAS Configuration Directory:";
			/**
			* text field labeled 'JonAS Installation Directory:'
			*/
			public static final String TEXT_JONAS_INSTALLATION_DIRECTORY = "JonAS Installation Directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			}
		public static class ApacheTomcatv50Server {
			/**
			* represents item : Apache->Tomcat v5.0 Server
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "Tomcat v5.0 Server";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class OracleOracleOC4JStandaloneServer1013 {
			/**
			* represents item : Oracle->Oracle OC4J Standalone Server 10.1.3
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "Oracle OC4J Standalone Server 10.1.3";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Oracle");
					return l;
				}
			};
			/**
			* text field labeled 'Oracle J2EE Home:'
			*/
			public static final String TEXT_ORACLE_J2EE_HOME = "Oracle J2EE Home:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			}
		public static class JBossCommunityJBossAS32 {
			/**
			* represents item : JBoss Community->JBoss AS 3.2
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JBoss AS 3.2";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class BasicJ2EEPreview {
			/**
			* represents item : Basic->J2EE Preview
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "J2EE Preview";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			/**
			* text field labeled 'Configured:'
			*/
			public static final String TEXT_CONFIGURED = "Configured:";
			/**
			* text field labeled 'Available:'
			*/
			public static final String TEXT_AVAILABLE = "Available:";
			}
		public static class OracleOracleOC4JStandaloneServer1013n {
			/**
			* represents item : Oracle->Oracle OC4J Standalone Server 10.1.3.n
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "Oracle OC4J Standalone Server 10.1.3.n";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Oracle");
					return l;
				}
			};
			/**
			* text field labeled 'Oracle J2EE Home:'
			*/
			public static final String TEXT_ORACLE_J2EE_HOME = "Oracle J2EE Home:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			}
		public static class JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform43 {
			/**
			* represents item : JBoss Enterprise Middleware->JBoss Enterprise Application Platform 4.3
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JBoss Enterprise Application Platform 4.3";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Enterprise Middleware");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class BasicHTTPPreview {
			/**
			* represents item : Basic->HTTP Preview
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "HTTP Preview";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			/**
			* text field labeled 'Configured:'
			*/
			public static final String TEXT_CONFIGURED = "Configured:";
			/**
			* text field labeled 'Available:'
			*/
			public static final String TEXT_AVAILABLE = "Available:";
			}
		public static class JBossCommunityJBossAS50 {
			/**
			* represents item : JBoss Community->JBoss AS 5.0
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JBoss AS 5.0";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossCommunityJBossAS51 {
			/**
			* represents item : JBoss Community->JBoss AS 5.1
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JBoss AS 5.1";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class BasicLocalDeployer {
			/**
			* represents item : Basic->Local Deployer
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "Local Deployer";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			}
		public static class IBMIBMWebSpherev60 {
			/**
			* represents item : IBM->IBM WebSphere v6.0
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "IBM WebSphere v6.0";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("IBM");
					return l;
				}
			};
			/**
			* text field labeled 'IBM WebSphere Installation Directory:'
			*/
			public static final String TEXT_IBM_WEBSPHERE_INSTALLATION_DIRECTORY = "IBM WebSphere Installation Directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			}
		public static class JBossCommunityJBossAS6x {
			/**
			* represents item : JBoss Community->JBoss AS 6.x
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JBoss AS 6.x";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossCommunityJBossAS70 {
			/**
			* represents item : JBoss Community->JBoss AS 7.0
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JBoss AS 7.0";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossCommunityJBossAS71 {
			/**
			* represents item : JBoss Community->JBoss AS 7.1
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JBoss AS 7.1";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class ApacheTomcatv32Server {
			/**
			* represents item : Apache->Tomcat v3.2 Server
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "Tomcat v3.2 Server";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class ApacheTomcatv40Server {
			/**
			* represents item : Apache->Tomcat v4.0 Server
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "Tomcat v4.0 Server";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class BasicSCPRemoteDeployer {
			/**
			* represents item : Basic->SCP Remote Deployer
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "SCP Remote Deployer";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			}
		public static class ApacheTomcatv41Server {
			/**
			* represents item : Apache->Tomcat v4.1 Server
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "Tomcat v4.1 Server";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class BasicHTTPServer {
			/**
			* represents item : Basic->HTTP Server
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "HTTP Server";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			/**
			* text field labeled 'Publishing Directory:'
			*/
			public static final String TEXT_PUBLISHING_DIRECTORY = "Publishing Directory:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class ApacheTomcatv60Server {
			/**
			* represents item : Apache->Tomcat v6.0 Server
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "Tomcat v6.0 Server";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class JBossCommunityJBossAS40 {
			/**
			* represents item : JBoss Community->JBoss AS 4.0
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JBoss AS 4.0";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform5x {
			/**
			* represents item : JBoss Enterprise Middleware->JBoss Enterprise Application Platform 5.x
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JBoss Enterprise Application Platform 5.x";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Enterprise Middleware");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossCommunityJBossAS42 {
			/**
			* represents item : JBoss Community->JBoss AS 4.2
			*/
			public static final IServer LABEL = new IServer() {
				public String getName() { return "JBoss AS 4.2";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}

		}

	public static class Perspective {
		public static class DROOLS {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Drools";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JBOSSAS {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "JBoss AS";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JAVAEE {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Java EE";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class CSSEDITING {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "CSS Editing";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JAVA {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Java";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class TEAMSYNCHRONIZING {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Team Synchronizing";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class DATABASEDEBUG {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Database Debug";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class SEAMDEFAULT {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Seam (default)";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class GUVNORREPOSITORYEXPLORING {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Guvnor Repository Exploring";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JBPMJPDL3 {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "jBPM jPDL 3";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class RESOURCE {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Resource";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JAVABROWSING {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Java Browsing";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class DATABASEDEVELOPMENT {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Database Development";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class SVNREPOSITORYEXPLORING {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "SVN Repository Exploring";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class CVSREPOSITORYEXPLORING {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "CVS Repository Exploring";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class DEBUG {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Debug";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JMX {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "JMX";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class PLUGINDEVELOPMENT {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Plug-in Development";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JPA {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "JPA";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class WEBDEVELOPMENT {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Web Development";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class WEB {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Web";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class HIBERNATE {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Hibernate";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class XML {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "XML";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JAVATYPEHIERARCHY {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "Java Type Hierarchy";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JAVASCRIPT {
			/**
			* represents item : 
			*/
			public static final IPerspective LABEL = new IPerspective() {
				public String getName() { return "JavaScript";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}

		}

	public static class NewObject {
		/**
		 * creates new action item instance from given path 
		 * @param path
		 * @return
		 */
		public static INewObject create(final String...path) {
			if (path.length<1) {
				throw new IllegalArgumentException("path must contain at least 1 item");
			}
			return new INewObject() {
		
				@Override
				public String getName() {
					// TODO Auto-generated method stub
					return path[path.length-1];
				}
		
				@Override
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					for (int i=0; i<path.length-1;i++) {
						l.add(path[i]);
					}
					return l;
				}
				
			};
		}
		public static class CVSProjectsfromCVS {
			/**
			* represents item : CVS->Projects from CVS
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Projects from CVS";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("CVS");
					return l;
				}
			};
			/**
			* text field labeled 'User:'
			*/
			public static final String TEXT_USER = "User:";
			/**
			* text field labeled 'Connection type:'
			*/
			public static final String TEXT_CONNECTION_TYPE = "Connection type:";
			/**
			* text field labeled 'Repository path:'
			*/
			public static final String TEXT_REPOSITORY_PATH = "Repository path:";
			/**
			* text field labeled 'Password:'
			*/
			public static final String TEXT_PASSWORD = "Password:";
			/**
			* text field labeled 'Host:'
			*/
			public static final String TEXT_HOST = "Host:";
			/**
			* checkbox field labeled 'Save password (could trigger secure storage login)'
			*/
			public static final String CHB_SAVE_PASSWORD_COULD_TRIGGER_SECURE_STORAGE_LOGIN = "Save password (could trigger secure storage login)";
			}
		public static class JBossjBPMjBPM3ProcessDefinition {
			/**
			* represents item : JBoss jBPM->jBPM 3 Process Definition
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "jBPM 3 Process Definition";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss jBPM");
					return l;
				}
			};
			/**
			* text field labeled 'Source folder :'
			*/
			public static final String TEXT_SOURCE_FOLDER_ = "Source folder :";
			/**
			* text field labeled 'Process name :'
			*/
			public static final String TEXT_PROCESS_NAME_ = "Process name :";
			}
		public static class UserAssistanceContextHelp {
			/**
			* represents item : User Assistance->Context Help
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Context Help";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("User Assistance");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class HibernateHibernateConsoleConfiguration {
			/**
			* represents item : Hibernate->Hibernate Console Configuration
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Hibernate Console Configuration";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Hibernate");
					return l;
				}
			};
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class WebServicesAntFiles {
			/**
			* represents item : Web Services->Ant Files
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Ant Files";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			/**
			* text field labeled 'Web service type:'
			*/
			public static final String TEXT_WEB_SERVICE_TYPE = "Web service type:";
			/**
			* text field labeled 'Web service runtime:'
			*/
			public static final String TEXT_WEB_SERVICE_RUNTIME = "Web service runtime:";
			/**
			* text field labeled 'Into folder:'
			*/
			public static final String TEXT_INTO_FOLDER = "Into folder:";
			}
		public static class JBossToolsWebJSFFacesConfig {
			/**
			* represents item : JBoss Tools Web->JSF->Faces Config
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Faces Config";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					l.add("JSF");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			/**
			* checkbox field labeled 'Register in web.xml'
			*/
			public static final String CHB_REGISTER_IN_WEBXML = "Register in web.xml";
			}
		public static class EclipseModelingFrameworkEMFGeneratorModel {
			/**
			* represents item : Eclipse Modeling Framework->EMF Generator Model
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "EMF Generator Model";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Eclipse Modeling Framework");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JBossToolsWebStrutsValidationFile {
			/**
			* represents item : JBoss Tools Web->Struts->Validation File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Validation File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					l.add("Struts");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			}
		public static class SeamSeamWebProject {
			/**
			* represents item : Seam->Seam Web Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Seam Web Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Seam");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Use default'
			*/
			public static final String CHB_USE_DEFAULT = "Use default";
			}
		public static class EJBMessageDrivenBeanEJB3x {
			/**
			* represents item : EJB->Message-Driven Bean (EJB 3.x)
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Message-Driven Bean (EJB 3.x)";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("EJB");
					return l;
				}
			};
			/**
			* text field labeled 'Destination type:'
			*/
			public static final String TEXT_DESTINATION_TYPE = "Destination type:";
			/**
			* text field labeled 'Java package:'
			*/
			public static final String TEXT_JAVA_PACKAGE = "Java package:";
			/**
			* text field labeled 'EJB project:'
			*/
			public static final String TEXT_EJB_PROJECT = "EJB project:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Destination name:'
			*/
			public static final String TEXT_DESTINATION_NAME = "Destination name:";
			/**
			* text field labeled 'Class name:'
			*/
			public static final String TEXT_CLASS_NAME = "Class name:";
			/**
			* checkbox field labeled 'JMS'
			*/
			public static final String CHB_JMS = "JMS";
			}
		public static class ConnectionProfilesConnectionProfile {
			/**
			* represents item : Connection Profiles->Connection Profile
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Connection Profile";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Connection Profiles");
					return l;
				}
			};
			/**
			* text field labeled 'Description (optional):'
			*/
			public static final String TEXT_DESCRIPTION_OPTIONAL = "Description (optional):";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class PluginDevelopmentExtensionPointSchema {
			/**
			* represents item : Plug-in Development->Extension Point Schema
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Extension Point Schema";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'Extension Point Schema:'
			*/
			public static final String TEXT_EXTENSION_POINT_SCHEMA = "Extension Point Schema:";
			/**
			* text field labeled 'Extension Point ID:'
			*/
			public static final String TEXT_EXTENSION_POINT_ID = "Extension Point ID:";
			/**
			* text field labeled 'Container:'
			*/
			public static final String TEXT_CONTAINER = "Container:";
			/**
			* text field labeled 'Plug-in ID:'
			*/
			public static final String TEXT_PLUGIN_ID = "Plug-in ID:";
			/**
			* text field labeled 'Extension Point Name:'
			*/
			public static final String TEXT_EXTENSION_POINT_NAME = "Extension Point Name:";
			/**
			* checkbox field labeled 'Create shared schema for inclusion'
			*/
			public static final String CHB_CREATE_SHARED_SCHEMA_FOR_INCLUSION = "Create shared schema for inclusion";
			/**
			* checkbox field labeled 'Edit extension point schema when done'
			*/
			public static final String CHB_EDIT_EXTENSION_POINT_SCHEMA_WHEN_DONE = "Edit extension point schema when done";
			}
		public static class JBossToolsWebJSFile {
			/**
			* represents item : JBoss Tools Web->JS File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JS File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			}
		public static class EJBSessionBeanEJB3x {
			/**
			* represents item : EJB->Session Bean (EJB 3.x)
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Session Bean (EJB 3.x)";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("EJB");
					return l;
				}
			};
			/**
			* text field labeled 'Java package:'
			*/
			public static final String TEXT_JAVA_PACKAGE = "Java package:";
			/**
			* text field labeled 'EJB project:'
			*/
			public static final String TEXT_EJB_PROJECT = "EJB project:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'State type:'
			*/
			public static final String TEXT_STATE_TYPE = "State type:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Class name:'
			*/
			public static final String TEXT_CLASS_NAME = "Class name:";
			/**
			* checkbox field labeled 'Local'
			*/
			public static final String CHB_LOCAL = "Local";
			/**
			* checkbox field labeled 'Remote'
			*/
			public static final String CHB_REMOTE = "Remote";
			}
		public static class JavaJavaRunDebugScrapbookPage {
			/**
			* represents item : Java->Java Run/Debug->Scrapbook Page
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Scrapbook Page";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Java Run/Debug");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class WebServicesWebServiceClient {
			/**
			* represents item : Web Services->Web Service Client
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Web Service Client";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			/**
			* text field labeled 'Client type:'
			*/
			public static final String TEXT_CLIENT_TYPE = "Client type:";
			/**
			* text field labeled 'Configuration:'
			*/
			public static final String TEXT_CONFIGURATION = "Configuration:";
			/**
			* text field labeled 'Service definition:'
			*/
			public static final String TEXT_SERVICE_DEFINITION = "Service definition:";
			/**
			* checkbox field labeled 'Monitor the Web service'
			*/
			public static final String CHB_MONITOR_THE_WEB_SERVICE = "Monitor the Web service";
			}
		public static class DroolsFlowFile {
			/**
			* represents item : Drools->Flow File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Flow File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class GeneralProject {
			/**
			* represents item : General->Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class DroolsDomainSpecificLanguage {
			/**
			* represents item : Drools->Domain Specific Language
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Domain Specific Language";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JBossToolsWebPortletJBossJSFSeamPortlet {
			/**
			* represents item : JBoss Tools Web->Portlet->JBoss JSF/Seam Portlet
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JBoss JSF/Seam Portlet";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					l.add("Portlet");
					return l;
				}
			};
			/**
			* text field labeled 'Project:'
			*/
			public static final String TEXT_PROJECT = "Project:";
			/**
			* text field labeled 'Class name:'
			*/
			public static final String TEXT_CLASS_NAME = "Class name:";
			}
		public static class JBossToolsCreateaSampleWebService {
			/**
			* represents item : JBoss Tools->Create a Sample Web Service
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Create a Sample Web Service";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					return l;
				}
			};
			}
		public static class JBossjBPMjBPM3Project {
			/**
			* represents item : JBoss jBPM->jBPM 3 Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "jBPM 3 Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss jBPM");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			}
		public static class PluginDevelopmentFragmentProject {
			/**
			* represents item : Plug-in Development->Fragment Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Fragment Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'This fragment is targeted to run with:'
			*/
			public static final String TEXT_THIS_FRAGMENT_IS_TARGETED_TO_RUN_WITH = "This fragment is targeted to run with:";
			/**
			* text field labeled 'Output folder:'
			*/
			public static final String TEXT_OUTPUT_FOLDER = "Output folder:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Create a Java project'
			*/
			public static final String CHB_CREATE_A_JAVA_PROJECT = "Create a Java project";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class JavaProjectfromExistingAntBuildfile {
			/**
			* represents item : Java Project from Existing Ant Buildfile
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Java Project from Existing Ant Buildfile";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			/**
			* text field labeled 'Select javac declaration to use to define project:'
			*/
			public static final String TEXT_SELECT_JAVAC_DECLARATION_TO_USE_TO_DEFINE_PROJECT = "Select javac declaration to use to define project:";
			/**
			* text field labeled 'Ant buildfile:'
			*/
			public static final String TEXT_ANT_BUILDFILE = "Ant buildfile:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* checkbox field labeled 'Link to the buildfile in the file system'
			*/
			public static final String CHB_LINK_TO_THE_BUILDFILE_IN_THE_FILE_SYSTEM = "Link to the buildfile in the file system";
			}
		public static class XMLDTD {
			/**
			* represents item : XML->DTD
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "DTD";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JavaEEEnterpriseApplicationProject {
			/**
			* represents item : Java EE->Enterprise Application Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Enterprise Application Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Use default'
			*/
			public static final String CHB_USE_DEFAULT = "Use default";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class ServerServer {
			/**
			* represents item : Server->Server
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Server";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Server");
					return l;
				}
			};
			/**
			* text field labeled 'Server name:'
			*/
			public static final String TEXT_SERVER_NAME = "Server name:";
			/**
			* text field labeled 'Select the server type:'
			*/
			public static final String TEXT_SELECT_THE_SERVER_TYPE = "Select the server type:";
			/**
			* text field labeled 'Server's host name:'
			*/
			public static final String TEXT_SERVERS_HOST_NAME = "Server's host name:";
			}
		public static class WebFilter {
			/**
			* represents item : Web->Filter
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Filter";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'Java package:'
			*/
			public static final String TEXT_JAVA_PACKAGE = "Java package:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Class name:'
			*/
			public static final String TEXT_CLASS_NAME = "Class name:";
			/**
			* text field labeled 'Web project:'
			*/
			public static final String TEXT_WEB_PROJECT = "Web project:";
			/**
			* checkbox field labeled 'Use existing Filter class'
			*/
			public static final String CHB_USE_EXISTING_FILTER_CLASS = "Use existing Filter class";
			}
		public static class SVNProjectfromSVN {
			/**
			* represents item : SVN->Project from SVN
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Project from SVN";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("SVN");
					return l;
				}
			};
			/**
			* text field labeled 'User:'
			*/
			public static final String TEXT_USER = "User:";
			/**
			* text field labeled 'Show Credentials For:'
			*/
			public static final String TEXT_SHOW_CREDENTIALS_FOR = "Show Credentials For:";
			/**
			* text field labeled 'URL:'
			*/
			public static final String TEXT_URL = "URL:";
			/**
			* text field labeled 'Password:'
			*/
			public static final String TEXT_PASSWORD = "Password:";
			/**
			* checkbox field labeled 'Save password'
			*/
			public static final String CHB_SAVE_PASSWORD = "Save password";
			/**
			* checkbox field labeled 'Validate Repository Location on finish'
			*/
			public static final String CHB_VALIDATE_REPOSITORY_LOCATION_ON_FINISH = "Validate Repository Location on finish";
			}
		public static class SQLDevelopmentSQLFile {
			/**
			* represents item : SQL Development->SQL File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "SQL File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("SQL Development");
					return l;
				}
			};
			/**
			* text field labeled 'Connection profile name:'
			*/
			public static final String TEXT_CONNECTION_PROFILE_NAME = "Connection profile name:";
			/**
			* text field labeled 'Database name:'
			*/
			public static final String TEXT_DATABASE_NAME = "Database name:";
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Database server type:'
			*/
			public static final String TEXT_DATABASE_SERVER_TYPE = "Database server type:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			/**
			* checkbox field labeled 'Do not connect now'
			*/
			public static final String CHB_DO_NOT_CONNECT_NOW = "Do not connect now";
			}
		public static class Class {
			/**
			* represents item : Class
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Class";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			/**
			* text field labeled 'Package:'
			*/
			public static final String TEXT_PACKAGE = "Package:";
			/**
			* text field labeled 'Modifiers:'
			*/
			public static final String TEXT_MODIFIERS = "Modifiers:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Interfaces:'
			*/
			public static final String TEXT_INTERFACES = "Interfaces:";
			/**
			* checkbox field labeled 'Constructors from superclass'
			*/
			public static final String CHB_CONSTRUCTORS_FROM_SUPERCLASS = "Constructors from superclass";
			/**
			* checkbox field labeled 'abstract'
			*/
			public static final String CHB_ABSTRACT = "abstract";
			/**
			* checkbox field labeled 'final'
			*/
			public static final String CHB_FINAL = "final";
			/**
			* checkbox field labeled 'static'
			*/
			public static final String CHB_STATIC = "static";
			/**
			* checkbox field labeled 'Enclosing type:'
			*/
			public static final String CHB_ENCLOSING_TYPE = "Enclosing type:";
			/**
			* checkbox field labeled 'public static void main(String[] args)'
			*/
			public static final String CHB_PUBLIC_STATIC_VOID_MAINSTRING_ARGS = "public static void main(String[] args)";
			/**
			* checkbox field labeled 'Generate comments'
			*/
			public static final String CHB_GENERATE_COMMENTS = "Generate comments";
			/**
			* checkbox field labeled 'Inherited abstract methods'
			*/
			public static final String CHB_INHERITED_ABSTRACT_METHODS = "Inherited abstract methods";
			}
		public static class JavaEmitterTemplatesConvertProjectstoJETProjects {
			/**
			* represents item : Java Emitter Templates->Convert Projects to JET Projects
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Convert Projects to JET Projects";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java Emitter Templates");
					return l;
				}
			};
			}
		public static class WebDynamicWebProject {
			/**
			* represents item : Web->Dynamic Web Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Dynamic Web Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'EAR project name:'
			*/
			public static final String TEXT_EAR_PROJECT_NAME = "EAR project name:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Use default'
			*/
			public static final String CHB_USE_DEFAULT = "Use default";
			/**
			* checkbox field labeled 'Add project to an EAR'
			*/
			public static final String CHB_ADD_PROJECT_TO_AN_EAR = "Add project to an EAR";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class DroolsDecisionTable {
			/**
			* represents item : Drools->Decision Table
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Decision Table";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JBossToolsProjectExamples {
			/**
			* represents item : JBoss Tools->Project Examples
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Project Examples";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					return l;
				}
			};
			/**
			* text field labeled 'Project size:'
			*/
			public static final String TEXT_PROJECT_SIZE = "Project size:";
			/**
			* text field labeled 'Projects:'
			*/
			public static final String TEXT_PROJECTS = "Projects:";
			/**
			* text field labeled 'Description:'
			*/
			public static final String TEXT_DESCRIPTION = "Description:";
			/**
			* text field labeled 'URL:'
			*/
			public static final String TEXT_URL = "URL:";
			/**
			* text field labeled 'Site:'
			*/
			public static final String TEXT_SITE = "Site:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* checkbox field labeled 'Show the Quick Fix dialog'
			*/
			public static final String CHB_SHOW_THE_QUICK_FIX_DIALOG = "Show the Quick Fix dialog";
			/**
			* checkbox field labeled 'Show experimental sites'
			*/
			public static final String CHB_SHOW_EXPERIMENTAL_SITES = "Show experimental sites";
			}
		public static class PluginDevelopmentFeatureProject {
			/**
			* represents item : Plug-in Development->Feature Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Feature Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'Feature Provider:'
			*/
			public static final String TEXT_FEATURE_PROVIDER = "Feature Provider:";
			/**
			* text field labeled 'Feature ID:'
			*/
			public static final String TEXT_FEATURE_ID = "Feature ID:";
			/**
			* text field labeled 'Feature Version:'
			*/
			public static final String TEXT_FEATURE_VERSION = "Feature Version:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Install Handler Library:'
			*/
			public static final String TEXT_INSTALL_HANDLER_LIBRARY = "Install Handler Library:";
			/**
			* text field labeled 'Feature Name:'
			*/
			public static final String TEXT_FEATURE_NAME = "Feature Name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			}
		public static class XMLXSL {
			/**
			* represents item : XML->XSL
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "XSL";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JavaJUnitJUnitTestSuite {
			/**
			* represents item : Java->JUnit->JUnit Test Suite
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JUnit Test Suite";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("JUnit");
					return l;
				}
			};
			/**
			* text field labeled 'Test classes to include in suite:'
			*/
			public static final String TEXT_TEST_CLASSES_TO_INCLUDE_IN_SUITE = "Test classes to include in suite:";
			/**
			* text field labeled 'Package:'
			*/
			public static final String TEXT_PACKAGE = "Package:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			}
		public static class SeamSeamEntity {
			/**
			* represents item : Seam->Seam Entity
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Seam Entity";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Seam");
					return l;
				}
			};
			/**
			* text field labeled 'Page name:'
			*/
			public static final String TEXT_PAGE_NAME = "Page name:";
			/**
			* text field labeled 'Package name:'
			*/
			public static final String TEXT_PACKAGE_NAME = "Package name:";
			/**
			* text field labeled 'Seam entity class name:'
			*/
			public static final String TEXT_SEAM_ENTITY_CLASS_NAME = "Seam entity class name:";
			/**
			* text field labeled 'Master page name:'
			*/
			public static final String TEXT_MASTER_PAGE_NAME = "Master page name:";
			/**
			* text field labeled 'Seam Project:'
			*/
			public static final String TEXT_SEAM_PROJECT = "Seam Project:";
			}
		public static class JavaEEConnectorProject {
			/**
			* represents item : Java EE->Connector Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Connector Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'EAR project name:'
			*/
			public static final String TEXT_EAR_PROJECT_NAME = "EAR project name:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Use default'
			*/
			public static final String CHB_USE_DEFAULT = "Use default";
			/**
			* checkbox field labeled 'Add project to an EAR'
			*/
			public static final String CHB_ADD_PROJECT_TO_AN_EAR = "Add project to an EAR";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class WebStaticWebProject {
			/**
			* represents item : Web->Static Web Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Static Web Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Use default'
			*/
			public static final String CHB_USE_DEFAULT = "Use default";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class JPAJPAProject {
			/**
			* represents item : JPA->JPA Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JPA Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JPA");
					return l;
				}
			};
			/**
			* text field labeled 'EAR project name:'
			*/
			public static final String TEXT_EAR_PROJECT_NAME = "EAR project name:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Use default'
			*/
			public static final String CHB_USE_DEFAULT = "Use default";
			/**
			* checkbox field labeled 'Add project to an EAR'
			*/
			public static final String CHB_ADD_PROJECT_TO_AN_EAR = "Add project to an EAR";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class JBossToolsWebCSSClass {
			/**
			* represents item : JBoss Tools Web->CSS Class
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "CSS Class";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			/**
			* text field labeled 'CSS Class*'
			*/
			public static final String TEXT_CSS_CLASS = "CSS Class*";
			/**
			* text field labeled 'CSS File*'
			*/
			public static final String TEXT_CSS_FILE = "CSS File*";
			}
		public static class JBossToolsWebStrutsStrutsConfig {
			/**
			* represents item : JBoss Tools Web->Struts->Struts Config
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Struts Config";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					l.add("Struts");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			/**
			* checkbox field labeled 'Register in web.xml'
			*/
			public static final String CHB_REGISTER_IN_WEBXML = "Register in web.xml";
			}
		public static class SmooksSmooksConfigurationFile {
			/**
			* represents item : Smooks->Smooks Configuration File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Smooks Configuration File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Smooks");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class EclipseModelingFrameworkEmptyEMFProject {
			/**
			* represents item : Eclipse Modeling Framework->Empty EMF Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Empty EMF Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Eclipse Modeling Framework");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			}
		public static class DroolsGuidedRule {
			/**
			* represents item : Drools->Guided Rule
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Guided Rule";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class PluginProject {
			/**
			* represents item : Plug-in Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Plug-in Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			/**
			* text field labeled 'This plug-in is targeted to run with:'
			*/
			public static final String TEXT_THIS_PLUGIN_IS_TARGETED_TO_RUN_WITH = "This plug-in is targeted to run with:";
			/**
			* text field labeled 'Output folder:'
			*/
			public static final String TEXT_OUTPUT_FOLDER = "Output folder:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Create a Java project'
			*/
			public static final String CHB_CREATE_A_JAVA_PROJECT = "Create a Java project";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class WebJSPFile {
			/**
			* represents item : JBoss Tools Web->JSP File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JSP File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'File name'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			}
		public static class SeamSeamConversation {
			/**
			* represents item : Seam->Seam Conversation
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Seam Conversation";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Seam");
					return l;
				}
			};
			/**
			* text field labeled 'Local interface name:'
			*/
			public static final String TEXT_LOCAL_INTERFACE_NAME = "Local interface name:";
			/**
			* text field labeled 'Seam component name:'
			*/
			public static final String TEXT_SEAM_COMPONENT_NAME = "Seam component name:";
			/**
			* text field labeled 'Package name:'
			*/
			public static final String TEXT_PACKAGE_NAME = "Package name:";
			/**
			* text field labeled 'Page name:'
			*/
			public static final String TEXT_PAGE_NAME = "Page name:";
			/**
			* text field labeled 'Method name:'
			*/
			public static final String TEXT_METHOD_NAME = "Method name:";
			/**
			* text field labeled 'Bean name:'
			*/
			public static final String TEXT_BEAN_NAME = "Bean name:";
			/**
			* text field labeled 'Seam Project:'
			*/
			public static final String TEXT_SEAM_PROJECT = "Seam Project:";
			}
		public static class SeamSeamAction {
			/**
			* represents item : Seam->Seam Action
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Seam Action";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Seam");
					return l;
				}
			};
			/**
			* text field labeled 'Local interface name:'
			*/
			public static final String TEXT_LOCAL_INTERFACE_NAME = "Local interface name:";
			/**
			* text field labeled 'Seam component name:'
			*/
			public static final String TEXT_SEAM_COMPONENT_NAME = "Seam component name:";
			/**
			* text field labeled 'Package name:'
			*/
			public static final String TEXT_PACKAGE_NAME = "Package name:";
			/**
			* text field labeled 'Page name:'
			*/
			public static final String TEXT_PAGE_NAME = "Page name:";
			/**
			* text field labeled 'Method name:'
			*/
			public static final String TEXT_METHOD_NAME = "Method name:";
			/**
			* text field labeled 'Bean name:'
			*/
			public static final String TEXT_BEAN_NAME = "Bean name:";
			/**
			* text field labeled 'Seam Project:'
			*/
			public static final String TEXT_SEAM_PROJECT = "Seam Project:";
			}
		public static class GuvnorGuvnorrepositorylocation {
			/**
			* represents item : Guvnor->Guvnor repository location
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Guvnor repository location";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Guvnor");
					return l;
				}
			};
			/**
			* text field labeled 'Port:'
			*/
			public static final String TEXT_PORT = "Port:";
			/**
			* text field labeled 'Repository:'
			*/
			public static final String TEXT_REPOSITORY = "Repository:";
			/**
			* text field labeled 'Password:'
			*/
			public static final String TEXT_PASSWORD = "Password:";
			/**
			* text field labeled 'User Name:'
			*/
			public static final String TEXT_USER_NAME = "User Name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			}
		public static class ESBESBAction {
			/**
			* represents item : ESB->ESB Action
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "ESB Action";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("ESB");
					return l;
				}
			};
			/**
			* text field labeled 'Package:'
			*/
			public static final String TEXT_PACKAGE = "Package:";
			/**
			* text field labeled 'Modifiers:'
			*/
			public static final String TEXT_MODIFIERS = "Modifiers:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Interfaces:'
			*/
			public static final String TEXT_INTERFACES = "Interfaces:";
			/**
			* checkbox field labeled 'Constructors from superclass'
			*/
			public static final String CHB_CONSTRUCTORS_FROM_SUPERCLASS = "Constructors from superclass";
			/**
			* checkbox field labeled 'abstract'
			*/
			public static final String CHB_ABSTRACT = "abstract";
			/**
			* checkbox field labeled 'final'
			*/
			public static final String CHB_FINAL = "final";
			/**
			* checkbox field labeled 'static'
			*/
			public static final String CHB_STATIC = "static";
			/**
			* checkbox field labeled 'Enclosing type:'
			*/
			public static final String CHB_ENCLOSING_TYPE = "Enclosing type:";
			/**
			* checkbox field labeled 'public static void main(String[] args)'
			*/
			public static final String CHB_PUBLIC_STATIC_VOID_MAINSTRING_ARGS = "public static void main(String[] args)";
			/**
			* checkbox field labeled 'Generate comments'
			*/
			public static final String CHB_GENERATE_COMMENTS = "Generate comments";
			/**
			* checkbox field labeled 'Inherited abstract methods'
			*/
			public static final String CHB_INHERITED_ABSTRACT_METHODS = "Inherited abstract methods";
			}
		public static class WebServicesUnitTestUDDI {
			/**
			* represents item : Web Services->Unit Test UDDI
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Unit Test UDDI";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			/**
			* text field labeled 'Private UDDI Registry type:'
			*/
			public static final String TEXT_PRIVATE_UDDI_REGISTRY_TYPE = "Private UDDI Registry type:";
			}
		public static class ExamplesXMLEditingandvalidatingXMLfiles {
			/**
			* represents item : Examples->XML->Editing and validating XML files
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Editing and validating XML files";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Examples");
					l.add("XML");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			}
		public static class ExampleEMFModelCreationWizardsXSDModel {
			/**
			* represents item : Example EMF Model Creation Wizards->XSD Model
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "XSD Model";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Example EMF Model Creation Wizards");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class UserAssistanceCheatSheet {
			/**
			* represents item : User Assistance->Cheat Sheet
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Cheat Sheet";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("User Assistance");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JavaEEUtilityProject {
			/**
			* represents item : Java EE->Utility Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Utility Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'EAR project name:'
			*/
			public static final String TEXT_EAR_PROJECT_NAME = "EAR project name:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Use default'
			*/
			public static final String CHB_USE_DEFAULT = "Use default";
			/**
			* checkbox field labeled 'Add project to an EAR'
			*/
			public static final String CHB_ADD_PROJECT_TO_AN_EAR = "Add project to an EAR";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class DroolsRuleResource {
			/**
			* represents item : Drools->Rule Resource
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Rule Resource";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			/**
			* text field labeled 'Use functions:'
			*/
			public static final String TEXT_USE_FUNCTIONS = "Use functions:";
			/**
			* text field labeled 'Type of rule resource:'
			*/
			public static final String TEXT_TYPE_OF_RULE_RESOURCE = "Type of rule resource:";
			/**
			* text field labeled 'Rule package name:'
			*/
			public static final String TEXT_RULE_PACKAGE_NAME = "Rule package name:";
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Use a DSL:'
			*/
			public static final String TEXT_USE_A_DSL = "Use a DSL:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JBossToolsMBeanComponentsMBeanStubs {
			/**
			* represents item : JBoss Tools->MBean Components->MBean Stubs
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "MBean Stubs";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("MBean Components");
					return l;
				}
			};
			/**
			* text field labeled 'Package:'
			*/
			public static final String TEXT_PACKAGE = "Package:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Extended interfaces:'
			*/
			public static final String TEXT_EXTENDED_INTERFACES = "Extended interfaces:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			}
		public static class JPAEclipseLinkEclipseLinkMappingFile {
			/**
			* represents item : JPA->EclipseLink->EclipseLink Mapping File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "EclipseLink Mapping File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JPA");
					l.add("EclipseLink");
					return l;
				}
			};
			/**
			* text field labeled 'Persistence unit:'
			*/
			public static final String TEXT_PERSISTENCE_UNIT = "Persistence unit:";
			/**
			* text field labeled 'Project:'
			*/
			public static final String TEXT_PROJECT = "Project:";
			/**
			* text field labeled 'Default access:'
			*/
			public static final String TEXT_DEFAULT_ACCESS = "Default access:";
			/**
			* text field labeled 'File path:'
			*/
			public static final String TEXT_FILE_PATH = "File path:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* checkbox field labeled 'Add to persistence unit'
			*/
			public static final String CHB_ADD_TO_PERSISTENCE_UNIT = "Add to persistence unit";
			}
		public static class JBossToolsWebStrutsStrutsProject {
			/**
			* represents item : JBoss Tools Web->Struts->Struts Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Struts Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					l.add("Struts");
					return l;
				}
			};
			/**
			* text field labeled 'Location*'
			*/
			public static final String TEXT_LOCATION = "Location*";
			/**
			* text field labeled 'Project Name*'
			*/
			public static final String TEXT_PROJECT_NAME = "Project Name*";
			/**
			* text field labeled 'Template*'
			*/
			public static final String TEXT_TEMPLATE = "Template*";
			/**
			* text field labeled 'Struts Environment*'
			*/
			public static final String TEXT_STRUTS_ENVIRONMENT = "Struts Environment*";
			/**
			* checkbox field labeled 'Use default path*'
			*/
			public static final String CHB_USE_DEFAULT_PATH = "Use default path*";
			}
		public static class SVNRepositoryLocation {
			/**
			* represents item : SVN->Repository Location
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Repository Location";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("SVN");
					return l;
				}
			};
			/**
			* text field labeled 'User:'
			*/
			public static final String TEXT_USER = "User:";
			/**
			* text field labeled 'Show Credentials For:'
			*/
			public static final String TEXT_SHOW_CREDENTIALS_FOR = "Show Credentials For:";
			/**
			* text field labeled 'URL:'
			*/
			public static final String TEXT_URL = "URL:";
			/**
			* text field labeled 'Password:'
			*/
			public static final String TEXT_PASSWORD = "Password:";
			/**
			* checkbox field labeled 'Save password'
			*/
			public static final String CHB_SAVE_PASSWORD = "Save password";
			/**
			* checkbox field labeled 'Validate Repository Location on finish'
			*/
			public static final String CHB_VALIDATE_REPOSITORY_LOCATION_ON_FINISH = "Validate Repository Location on finish";
			}
		public static class JavaScriptJavaScriptSourceFile {
			/**
			* represents item : JavaScript->JavaScript Source File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JavaScript Source File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JavaScript");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JPAMappingFile {
			/**
			* represents item : JPA->Mapping File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Mapping File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JPA");
					return l;
				}
			};
			/**
			* text field labeled 'Persistence unit:'
			*/
			public static final String TEXT_PERSISTENCE_UNIT = "Persistence unit:";
			/**
			* text field labeled 'Project:'
			*/
			public static final String TEXT_PROJECT = "Project:";
			/**
			* text field labeled 'Default access:'
			*/
			public static final String TEXT_DEFAULT_ACCESS = "Default access:";
			/**
			* text field labeled 'File path:'
			*/
			public static final String TEXT_FILE_PATH = "File path:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* checkbox field labeled 'Add to persistence unit'
			*/
			public static final String CHB_ADD_TO_PERSISTENCE_UNIT = "Add to persistence unit";
			}
		public static class XMLXMLSchema {
			/**
			* represents item : XML->XML Schema
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "XML Schema";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class EclipseModelingFrameworkMappingEcoretoXMLMapping {
			/**
			* represents item : Eclipse Modeling Framework->Mapping->Ecore to XML Mapping
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Ecore to XML Mapping";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Eclipse Modeling Framework");
					l.add("Mapping");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class PluginDevelopmentProductConfiguration {
			/**
			* represents item : Plug-in Development->Product Configuration
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Product Configuration";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class SeamSeamComponentsFile {
			/**
			* represents item : Seam->Seam Components File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Seam Components File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Seam");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Version*'
			*/
			public static final String TEXT_VERSION = "Version*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			}
		public static class WebJSP {
			/**
			* represents item : Web->JSP
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JSP File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JavaAnnotation {
			/**
			* represents item : Java->Annotation
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Annotation";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			/**
			* text field labeled 'Package:'
			*/
			public static final String TEXT_PACKAGE = "Package:";
			/**
			* text field labeled 'Modifiers:'
			*/
			public static final String TEXT_MODIFIERS = "Modifiers:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* checkbox field labeled 'Enclosing type:'
			*/
			public static final String CHB_ENCLOSING_TYPE = "Enclosing type:";
			/**
			* checkbox field labeled 'Generate comments'
			*/
			public static final String CHB_GENERATE_COMMENTS = "Generate comments";
			}
		public static class SeamSeamGenerateEntities {
			/**
			* represents item : Seam->Seam Generate Entities
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Seam Generate Entities";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Seam");
					return l;
				}
			};
			/**
			* text field labeled 'Hibernate Console Configuration:'
			*/
			public static final String TEXT_HIBERNATE_CONSOLE_CONFIGURATION = "Hibernate Console Configuration:";
			/**
			* text field labeled 'Seam Project:'
			*/
			public static final String TEXT_SEAM_PROJECT = "Seam Project:";
			}
		public static class HibernateHibernateConfigurationFilecfgxml {
			/**
			* represents item : Hibernate->Hibernate Configuration File (cfg.xml)
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Hibernate Configuration File (cfg.xml)";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Hibernate");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class XMLXML {
			/**
			* represents item : XML->XML
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "XML";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class SeamSeamForm {
			/**
			* represents item : Seam->Seam Form
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Seam Form";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Seam");
					return l;
				}
			};
			/**
			* text field labeled 'Local interface name:'
			*/
			public static final String TEXT_LOCAL_INTERFACE_NAME = "Local interface name:";
			/**
			* text field labeled 'Seam component name:'
			*/
			public static final String TEXT_SEAM_COMPONENT_NAME = "Seam component name:";
			/**
			* text field labeled 'Package name:'
			*/
			public static final String TEXT_PACKAGE_NAME = "Package name:";
			/**
			* text field labeled 'Page name:'
			*/
			public static final String TEXT_PAGE_NAME = "Page name:";
			/**
			* text field labeled 'Method name:'
			*/
			public static final String TEXT_METHOD_NAME = "Method name:";
			/**
			* text field labeled 'Bean name:'
			*/
			public static final String TEXT_BEAN_NAME = "Bean name:";
			/**
			* text field labeled 'Seam Project:'
			*/
			public static final String TEXT_SEAM_PROJECT = "Seam Project:";
			}
		public static class EJBEJB3SessionBean {
			/**
			* represents item : EJB->EJB3 Session Bean
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "EJB3 Session Bean";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("EJB");
					return l;
				}
			};
			/**
			* text field labeled 'Bean Class Name:'
			*/
			public static final String TEXT_BEAN_CLASS_NAME = "Bean Class Name:";
			/**
			* text field labeled 'Bean Package:'
			*/
			public static final String TEXT_BEAN_PACKAGE = "Bean Package:";
			/**
			* text field labeled 'Remote Interface Name:'
			*/
			public static final String TEXT_REMOTE_INTERFACE_NAME = "Remote Interface Name:";
			/**
			* text field labeled 'Modifiers:'
			*/
			public static final String TEXT_MODIFIERS = "Modifiers:";
			/**
			* text field labeled 'Bean Name:'
			*/
			public static final String TEXT_BEAN_NAME = "Bean Name:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Session Bean Type:'
			*/
			public static final String TEXT_SESSION_BEAN_TYPE = "Session Bean Type:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Interfaces:'
			*/
			public static final String TEXT_INTERFACES = "Interfaces:";
			/**
			* checkbox field labeled 'abstract'
			*/
			public static final String CHB_ABSTRACT = "abstract";
			/**
			* checkbox field labeled 'final'
			*/
			public static final String CHB_FINAL = "final";
			/**
			* checkbox field labeled 'static'
			*/
			public static final String CHB_STATIC = "static";
			/**
			* checkbox field labeled 'Enclosing type:'
			*/
			public static final String CHB_ENCLOSING_TYPE = "Enclosing type:";
			/**
			* checkbox field labeled 'Remote Interface Package:'
			*/
			public static final String CHB_REMOTE_INTERFACE_PACKAGE = "Remote Interface Package:";
			}
		public static class HibernateHibernateXMLMappingfilehbmxml {
			/**
			* represents item : Hibernate->Hibernate XML Mapping file (hbm.xml)
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Hibernate XML Mapping file (hbm.xml)";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Hibernate");
					return l;
				}
			};
			/**
			* checkbox field labeled 'depth control'
			*/
			public static final String CHB_DEPTH_CONTROL = "depth control";
			}
		public static class ESBESBFile {
			/**
			* represents item : ESB->ESB File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "ESB File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("ESB");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder:*";
			/**
			* text field labeled 'Version*'
			*/
			public static final String TEXT_VERSION = "Version:*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name:*";
			}
		public static class EJBEJB3MessageDrivenBean {
			/**
			* represents item : EJB->EJB3 Message Driven Bean
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "EJB3 Message Driven Bean";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("EJB");
					return l;
				}
			};
			/**
			* text field labeled 'Package:'
			*/
			public static final String TEXT_PACKAGE = "Package:";
			/**
			* text field labeled 'Modifiers:'
			*/
			public static final String TEXT_MODIFIERS = "Modifiers:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Interfaces:'
			*/
			public static final String TEXT_INTERFACES = "Interfaces:";
			/**
			* checkbox field labeled 'abstract'
			*/
			public static final String CHB_ABSTRACT = "abstract";
			/**
			* checkbox field labeled 'final'
			*/
			public static final String CHB_FINAL = "final";
			/**
			* checkbox field labeled 'static'
			*/
			public static final String CHB_STATIC = "static";
			/**
			* checkbox field labeled 'Enclosing type:'
			*/
			public static final String CHB_ENCLOSING_TYPE = "Enclosing type:";
			}
		public static class SeamSeamPageFlow {
			/**
			* represents item : Seam->Seam Page Flow
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Seam Page Flow";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Seam");
					return l;
				}
			};
			/**
			* text field labeled 'File name :'
			*/
			public static final String TEXT_FILE_NAME_ = "File name :";
			/**
			* text field labeled 'Source folder :'
			*/
			public static final String TEXT_SOURCE_FOLDER_ = "Source folder :";
			}
		public static class WebServicesWebService {
			/**
			* represents item : Web Services->Web Service
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Web Service";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			/**
			* text field labeled 'Client type:'
			*/
			public static final String TEXT_CLIENT_TYPE = "Client type:";
			/**
			* text field labeled 'Service implementation:'
			*/
			public static final String TEXT_SERVICE_IMPLEMENTATION = "Service implementation:";
			/**
			* text field labeled 'Web service type:'
			*/
			public static final String TEXT_WEB_SERVICE_TYPE = "Web service type:";
			/**
			* text field labeled 'Configuration:'
			*/
			public static final String TEXT_CONFIGURATION = "Configuration:";
			/**
			* checkbox field labeled 'Publish the Web service'
			*/
			public static final String CHB_PUBLISH_THE_WEB_SERVICE = "Publish the Web service";
			/**
			* checkbox field labeled 'Monitor the Web service'
			*/
			public static final String CHB_MONITOR_THE_WEB_SERVICE = "Monitor the Web service";
			}
		public static class WebServlet {
			/**
			* represents item : Web->Servlet
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Servlet";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'Java package:'
			*/
			public static final String TEXT_JAVA_PACKAGE = "Java package:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Class name:'
			*/
			public static final String TEXT_CLASS_NAME = "Class name:";
			/**
			* text field labeled 'Web project:'
			*/
			public static final String TEXT_WEB_PROJECT = "Web project:";
			/**
			* checkbox field labeled 'Use an existing Servlet class or JSP'
			*/
			public static final String CHB_USE_AN_EXISTING_SERVLET_CLASS_OR_JSP = "Use an existing Servlet class or JSP";
			}
		public static class WebHTMLPage {
			/**
			* represents item : Web->HTML Page
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "HTML File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JavaEnum {
			/**
			* represents item : Java->Enum
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Enum";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			/**
			* text field labeled 'Package:'
			*/
			public static final String TEXT_PACKAGE = "Package:";
			/**
			* text field labeled 'Modifiers:'
			*/
			public static final String TEXT_MODIFIERS = "Modifiers:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Interfaces:'
			*/
			public static final String TEXT_INTERFACES = "Interfaces:";
			/**
			* checkbox field labeled 'Enclosing type:'
			*/
			public static final String CHB_ENCLOSING_TYPE = "Enclosing type:";
			/**
			* checkbox field labeled 'Generate comments'
			*/
			public static final String CHB_GENERATE_COMMENTS = "Generate comments";
			}
		public static class HibernateHibernateReverseEngineeringFilerevengxml {
			/**
			* represents item : Hibernate->Hibernate Reverse Engineering File (reveng.xml)
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Hibernate Reverse Engineering File (reveng.xml)";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Hibernate");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class WebServicesWSDL {
			/**
			* represents item : Web Services->WSDL
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "WSDL";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class GuvnorResourcesfromGuvnor {
			/**
			* represents item : Guvnor->Resources from Guvnor
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Resources from Guvnor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Guvnor");
					return l;
				}
			};
			}
		public static class JBossToolsWebTLDFile {
			/**
			* represents item : JBoss Tools Web->TLD File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "TLD File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			/**
			* text field labeled 'Tlibversion*'
			*/
			public static final String TEXT_TLIBVERSION = "Tlibversion*";
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			/**
			* text field labeled 'Shortname*'
			*/
			public static final String TEXT_SHORTNAME = "Shortname*";
			/**
			* text field labeled 'Jspversion*'
			*/
			public static final String TEXT_JSPVERSION = "Jspversion*";
			}
		public static class JBossToolsWebXHTMLFile {
			/**
			* represents item : JBoss Tools Web->XHTML File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "XHTML Page";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JBossToolsWebHTMLFile {
			/**
			* represents item : JBoss Tools Web->HTML File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "HTML File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			}
		public static class WebCSS {
			/**
			* represents item : Web->CSS
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "CSS File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class SpringSpringProject {
			/**
			* represents item : Spring->Spring Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Spring Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					return l;
				}
			};
			/**
			* text field labeled 'Source Folder Name:'
			*/
			public static final String TEXT_SOURCE_FOLDER_NAME = "Source Folder Name:";
			/**
			* text field labeled 'Output Folder Name:'
			*/
			public static final String TEXT_OUTPUT_FOLDER_NAME = "Output Folder Name:";
			/**
			* text field labeled 'List of Config file suffixes:'
			*/
			public static final String TEXT_LIST_OF_CONFIG_FILE_SUFFIXES = "List of Config file suffixes:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Enable new project for Project Facets'
			*/
			public static final String CHB_ENABLE_NEW_PROJECT_FOR_PROJECT_FACETS = "Enable new project for Project Facets";
			/**
			* checkbox field labeled 'Create a Java project'
			*/
			public static final String CHB_CREATE_A_JAVA_PROJECT = "Create a Java project";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			}
		public static class JavaSourceFolder {
			/**
			* represents item : Java->Source Folder
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Source Folder";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			/**
			* text field labeled 'Folder name:'
			*/
			public static final String TEXT_FOLDER_NAME = "Folder name:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* checkbox field labeled 'Update exclusion filters in other source folders to solve nesting'
			*/
			public static final String CHB_UPDATE_EXCLUSION_FILTERS_IN_OTHER_SOURCE_FOLDERS_TO_SOLVE_NESTING = "Update exclusion filters in other source folders to solve nesting";
			}
		public static class Interface {
			/**
			* represents item : Interface
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Interface";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			/**
			* text field labeled 'Package:'
			*/
			public static final String TEXT_PACKAGE = "Package:";
			/**
			* text field labeled 'Modifiers:'
			*/
			public static final String TEXT_MODIFIERS = "Modifiers:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Extended interfaces:'
			*/
			public static final String TEXT_EXTENDED_INTERFACES = "Extended interfaces:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* checkbox field labeled 'Enclosing type:'
			*/
			public static final String CHB_ENCLOSING_TYPE = "Enclosing type:";
			/**
			* checkbox field labeled 'Generate comments'
			*/
			public static final String CHB_GENERATE_COMMENTS = "Generate comments";
			}
		public static class ESBESBProject {
			/**
			* represents item : ESB->ESB Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "ESB Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("ESB");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Use default'
			*/
			public static final String CHB_USE_DEFAULT = "Use default";
			}
		public static class JPAEntitiesFromTables {
			/**
			* represents item : JPA->Entities From Tables
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Entities From Tables";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JPA");
					return l;
				}
			};
			}
		public static class JavaEEApplicationClientProject {
			/**
			* represents item : Java EE->Application Client Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Application Client Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'EAR project name:'
			*/
			public static final String TEXT_EAR_PROJECT_NAME = "EAR project name:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Use default'
			*/
			public static final String CHB_USE_DEFAULT = "Use default";
			/**
			* checkbox field labeled 'Add project to an EAR'
			*/
			public static final String CHB_ADD_PROJECT_TO_AN_EAR = "Add project to an EAR";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class JavaScriptJavaScriptProject {
			/**
			* represents item : JavaScript->JavaScript Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JavaScript Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JavaScript");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Include ECMA 3 Browser Library'
			*/
			public static final String CHB_INCLUDE_ECMA_3_BROWSER_LIBRARY = "Include ECMA 3 Browser Library";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			/**
			* checkbox field labeled 'Use Window as the default SuperType   '
			*/
			public static final String CHB_USE_WINDOW_AS_THE_DEFAULT_SUPERTYPE___ = "Use Window as the default SuperType   ";
			}
		public static class JBossToolsWebPortletJavaPortlet {
			/**
			* represents item : JBoss Tools Web->Portlet->Java Portlet
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Java Portlet";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					l.add("Portlet");
					return l;
				}
			};
			/**
			* text field labeled 'Java package:'
			*/
			public static final String TEXT_JAVA_PACKAGE = "Java package:";
			/**
			* text field labeled 'Project:'
			*/
			public static final String TEXT_PROJECT = "Project:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Class name:'
			*/
			public static final String TEXT_CLASS_NAME = "Class name:";
			/**
			* checkbox field labeled 'Use an existing Portlet class'
			*/
			public static final String CHB_USE_AN_EXISTING_PORTLET_CLASS = "Use an existing Portlet class";
			}
		public static class JPAEntity {
			/**
			* represents item : JPA->Entity
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Entity";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JPA");
					return l;
				}
			};
			/**
			* text field labeled 'Java package:'
			*/
			public static final String TEXT_JAVA_PACKAGE = "Java package:";
			/**
			* text field labeled 'Project:'
			*/
			public static final String TEXT_PROJECT = "Project:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Mapping file:'
			*/
			public static final String TEXT_MAPPING_FILE = "Mapping file:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Class name:'
			*/
			public static final String TEXT_CLASS_NAME = "Class name:";
			/**
			* checkbox field labeled 'Add to entity mappings in XML'
			*/
			public static final String CHB_ADD_TO_ENTITY_MAPPINGS_IN_XML = "Add to entity mappings in XML";
			/**
			* checkbox field labeled 'Inheritance:'
			*/
			public static final String CHB_INHERITANCE = "Inheritance:";
			}
		public static class PluginDevelopmentPluginfromExistingJARArchives {
			/**
			* represents item : Plug-in Development->Plug-in from Existing JAR Archives
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Plug-in from Existing JAR Archives";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'JAR archives to include in the plug-in:'
			*/
			public static final String TEXT_JAR_ARCHIVES_TO_INCLUDE_IN_THE_PLUGIN = "JAR archives to include in the plug-in:";
			}
		public static class CVSCVSRepositoryLocation {
			/**
			* represents item : CVS->CVS Repository Location
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "CVS Repository Location";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("CVS");
					return l;
				}
			};
			/**
			* text field labeled 'User:'
			*/
			public static final String TEXT_USER = "User:";
			/**
			* text field labeled 'Connection type:'
			*/
			public static final String TEXT_CONNECTION_TYPE = "Connection type:";
			/**
			* text field labeled 'Repository path:'
			*/
			public static final String TEXT_REPOSITORY_PATH = "Repository path:";
			/**
			* text field labeled 'Password:'
			*/
			public static final String TEXT_PASSWORD = "Password:";
			/**
			* text field labeled 'Host:'
			*/
			public static final String TEXT_HOST = "Host:";
			/**
			* checkbox field labeled 'Save password (could trigger secure storage login)'
			*/
			public static final String CHB_SAVE_PASSWORD_COULD_TRIGGER_SECURE_STORAGE_LOGIN = "Save password (could trigger secure storage login)";
			/**
			* checkbox field labeled 'Validate connection on finish'
			*/
			public static final String CHB_VALIDATE_CONNECTION_ON_FINISH = "Validate connection on finish";
			}
		public static class EclipseModelingFrameworkEcoreModel {
			/**
			* represents item : Eclipse Modeling Framework->Ecore Model
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Ecore Model";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Eclipse Modeling Framework");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class WebJSPTag {
			/**
			* represents item : Web->JSP Tag
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JSP Tag";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class SpringSpringWebFlowDefinitionFile {
			/**
			* represents item : Spring->Spring Web Flow Definition File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Spring Web Flow Definition File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			/**
			* checkbox field labeled 'Add Spring project nature if required'
			*/
			public static final String CHB_ADD_SPRING_PROJECT_NATURE_IF_REQUIRED = "Add Spring project nature if required";
			/**
			* checkbox field labeled 'Use Spring Web Flow 2 flow definition syntax (Required if you want to use Spring Web Flow 2)'
			*/
			public static final String CHB_USE_SPRING_WEB_FLOW_2_FLOW_DEFINITION_SYNTAX_REQUIRED_IF_YOU_WANT_TO_USE_SPRING_WEB_FLOW_2 = "Use Spring Web Flow 2 flow definition syntax (Required if you want to use Spring Web Flow 2)";
			}
		public static class JBossToolsWebStrutsTilesFile {
			/**
			* represents item : JBoss Tools Web->Struts->Tiles File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Tiles File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					l.add("Struts");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			/**
			* checkbox field labeled 'Register'
			*/
			public static final String CHB_REGISTER = "Register";
			}
		public static class PluginDevelopmentTargetDefinition {
			/**
			* represents item : Plug-in Development->Target Definition
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Target Definition";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'Initialize the target definition with:'
			*/
			public static final String TEXT_INITIALIZE_THE_TARGET_DEFINITION_WITH = "Initialize the target definition with:";
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JBossToolsWebWebDescriptor {
			/**
			* represents item : JBoss Tools Web->Web Descriptor
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Web Descriptor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			}
		public static class PluginDevelopmentUpdateSiteProject {
			/**
			* represents item : Plug-in Development->Update Site Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Update Site Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'Web resources location:'
			*/
			public static final String TEXT_WEB_RESOURCES_LOCATION = "Web resources location:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			/**
			* checkbox field labeled 'Generate a web page listing all available features within the site'
			*/
			public static final String CHB_GENERATE_A_WEB_PAGE_LISTING_ALL_AVAILABLE_FEATURES_WITHIN_THE_SITE = "Generate a web page listing all available features within the site";
			}
		public static class EJBEJBProject {
			/**
			* represents item : EJB->EJB Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "EJB Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("EJB");
					return l;
				}
			};
			/**
			* text field labeled 'EAR project name:'
			*/
			public static final String TEXT_EAR_PROJECT_NAME = "EAR project name:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Use default'
			*/
			public static final String CHB_USE_DEFAULT = "Use default";
			/**
			* checkbox field labeled 'Add project to an EAR'
			*/
			public static final String CHB_ADD_PROJECT_TO_AN_EAR = "Add project to an EAR";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class JavaProject {
			/**
			* represents item : Java Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Java Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class EJBXDocletEnterpriseJavaBean {
			/**
			* represents item : EJB->XDoclet Enterprise JavaBean
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "XDoclet Enterprise JavaBean";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("EJB");
					return l;
				}
			};
			}
		public static class JBossjBPMjBPM3ActionHandler {
			/**
			* represents item : JBoss jBPM->jBPM 3 Action Handler
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "jBPM 3 Action Handler";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss jBPM");
					return l;
				}
			};
			/**
			* text field labeled 'Package:'
			*/
			public static final String TEXT_PACKAGE = "Package:";
			/**
			* text field labeled 'Modifiers:'
			*/
			public static final String TEXT_MODIFIERS = "Modifiers:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Interfaces:'
			*/
			public static final String TEXT_INTERFACES = "Interfaces:";
			/**
			* checkbox field labeled 'Constructors from superclass'
			*/
			public static final String CHB_CONSTRUCTORS_FROM_SUPERCLASS = "Constructors from superclass";
			/**
			* checkbox field labeled 'abstract'
			*/
			public static final String CHB_ABSTRACT = "abstract";
			/**
			* checkbox field labeled 'final'
			*/
			public static final String CHB_FINAL = "final";
			/**
			* checkbox field labeled 'static'
			*/
			public static final String CHB_STATIC = "static";
			/**
			* checkbox field labeled 'Enclosing type:'
			*/
			public static final String CHB_ENCLOSING_TYPE = "Enclosing type:";
			/**
			* checkbox field labeled 'public static void main(String[] args)'
			*/
			public static final String CHB_PUBLIC_STATIC_VOID_MAINSTRING_ARGS = "public static void main(String[] args)";
			/**
			* checkbox field labeled 'Generate comments'
			*/
			public static final String CHB_GENERATE_COMMENTS = "Generate comments";
			/**
			* checkbox field labeled 'Inherited abstract methods'
			*/
			public static final String CHB_INHERITED_ABSTRACT_METHODS = "Inherited abstract methods";
			}
		public static class JavaPackage {
			/**
			* represents item : Java->Package
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Package";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			}
		public static class JBossToolsWebPropertiesFile {
			/**
			* represents item : JBoss Tools Web->Properties File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Properties File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			}
		public static class JBossToolsWebCSSFile {
			/**
			* represents item : JBoss Tools Web->CSS File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "CSS File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					return l;
				}
			};
			/**
			* text field labeled 'Folder*'
			*/
			public static final String TEXT_FOLDER = "Folder*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			}
		public static class PluginDevelopmentCategoryDefinition {
			/**
			* represents item : Plug-in Development->Category Definition
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Category Definition";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class GeneralFacetedProject {
			/**
			* represents item : General->Faceted Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Faceted Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class PluginDevelopmentFeaturePatch {
			/**
			* represents item : Plug-in Development->Feature Patch
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Feature Patch";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'Feature ID:'
			*/
			public static final String TEXT_FEATURE_ID = "Feature ID:";
			/**
			* text field labeled 'Patch ID:'
			*/
			public static final String TEXT_PATCH_ID = "Patch ID:";
			/**
			* text field labeled 'Patch Name:'
			*/
			public static final String TEXT_PATCH_NAME = "Patch Name:";
			/**
			* text field labeled 'Feature Version:'
			*/
			public static final String TEXT_FEATURE_VERSION = "Feature Version:";
			/**
			* text field labeled 'Patch Provider:'
			*/
			public static final String TEXT_PATCH_PROVIDER = "Patch Provider:";
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Install Handler Library:'
			*/
			public static final String TEXT_INSTALL_HANDLER_LIBRARY = "Install Handler Library:";
			/**
			* text field labeled 'Feature Name:'
			*/
			public static final String TEXT_FEATURE_NAME = "Feature Name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			}
		public static class SpringSpringBeanConfigurationFile {
			/**
			* represents item : Spring->Spring Bean Configuration File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Spring Bean Configuration File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			/**
			* checkbox field labeled 'Add Spring project nature if required'
			*/
			public static final String CHB_ADD_SPRING_PROJECT_NATURE_IF_REQUIRED = "Add Spring project nature if required";
			}
		public static class GeneralFolder {
			/**
			* represents item : General->Folder
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Folder";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'Folder name:'
			*/
			public static final String TEXT_FOLDER_NAME = "Folder name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JBossToolsWebJSFJSFProject {
			/**
			* represents item : JBoss Tools Web->JSF->JSF Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JSF Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools Web");
					l.add("JSF");
					return l;
				}
			};
			/**
			* text field labeled 'Location*'
			*/
			public static final String TEXT_LOCATION = "Location*";
			/**
			* text field labeled 'Project Name*'
			*/
			public static final String TEXT_PROJECT_NAME = "Project Name*";
			/**
			* text field labeled 'Template*'
			*/
			public static final String TEXT_TEMPLATE = "Template*";
			/**
			* text field labeled 'JSF Environment*'
			*/
			public static final String TEXT_JSF_ENVIRONMENT = "JSF Environment*";
			/**
			* checkbox field labeled 'Use default path*'
			*/
			public static final String CHB_USE_DEFAULT_PATH = "Use default path*";
			}
		public static class JavaJavaWorkingSet {
			/**
			* represents item : Java->Java Working Set
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Java Working Set";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			/**
			* text field labeled 'Working set name:'
			*/
			public static final String TEXT_WORKING_SET_NAME = "Working set name:";
			/**
			* text field labeled 'Workspace content:'
			*/
			public static final String TEXT_WORKSPACE_CONTENT = "Workspace content:";
			/**
			* text field labeled 'Working set content:'
			*/
			public static final String TEXT_WORKING_SET_CONTENT = "Working set content:";
			}
		public static class JavaJUnitJUnitTestCase {
			/**
			* represents item : Java->JUnit->JUnit Test Case
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "JUnit Test Case";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("JUnit");
					return l;
				}
			};
			/**
			* text field labeled 'Package:'
			*/
			public static final String TEXT_PACKAGE = "Package:";
			/**
			* text field labeled 'Class under test:'
			*/
			public static final String TEXT_CLASS_UNDER_TEST = "Class under test:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* checkbox field labeled 'tearDown()'
			*/
			public static final String CHB_TEARDOWN = "tearDown()";
			/**
			* checkbox field labeled 'setUpBeforeClass()'
			*/
			public static final String CHB_SETUPBEFORECLASS = "setUpBeforeClass()";
			/**
			* checkbox field labeled 'Generate comments'
			*/
			public static final String CHB_GENERATE_COMMENTS = "Generate comments";
			/**
			* checkbox field labeled 'setUp()'
			*/
			public static final String CHB_SETUP = "setUp()";
			/**
			* checkbox field labeled 'tearDownAfterClass()'
			*/
			public static final String CHB_TEARDOWNAFTERCLASS = "tearDownAfterClass()";
			/**
			* checkbox field labeled 'constructor'
			*/
			public static final String CHB_CONSTRUCTOR = "constructor";
			}
		public static class GeneralFile {
			/**
			* represents item : General->File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class JPAEclipseLinkEclipseLink11MappingFile {
			/**
			* represents item : JPA->EclipseLink->EclipseLink 1.1 Mapping File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "EclipseLink 1.1 Mapping File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JPA");
					l.add("EclipseLink");
					return l;
				}
			};
			/**
			* text field labeled 'Persistence unit:'
			*/
			public static final String TEXT_PERSISTENCE_UNIT = "Persistence unit:";
			/**
			* text field labeled 'Project:'
			*/
			public static final String TEXT_PROJECT = "Project:";
			/**
			* text field labeled 'Default access:'
			*/
			public static final String TEXT_DEFAULT_ACCESS = "Default access:";
			/**
			* text field labeled 'File path:'
			*/
			public static final String TEXT_FILE_PATH = "File path:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* checkbox field labeled 'Add to persistence unit'
			*/
			public static final String CHB_ADD_TO_PERSISTENCE_UNIT = "Add to persistence unit";
			}
		public static class UserAssistanceHelpTableofContents {
			/**
			* represents item : User Assistance->Help Table of Contents
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Help Table of Contents";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("User Assistance");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class ConnectionProfilesConnectionProfileRepository {
			/**
			* represents item : Connection Profiles->Connection Profile Repository
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Connection Profile Repository";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Connection Profiles");
					return l;
				}
			};
			/**
			* text field labeled 'Description (optional):'
			*/
			public static final String TEXT_DESCRIPTION_OPTIONAL = "Description (optional):";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class DroolsDroolsProject {
			/**
			* represents item : Drools->Drools Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Drools Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			}
		public static class WebListener {
			/**
			* represents item : Web->Listener
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Listener";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'Java package:'
			*/
			public static final String TEXT_JAVA_PACKAGE = "Java package:";
			/**
			* text field labeled 'Superclass:'
			*/
			public static final String TEXT_SUPERCLASS = "Superclass:";
			/**
			* text field labeled 'Source folder:'
			*/
			public static final String TEXT_SOURCE_FOLDER = "Source folder:";
			/**
			* text field labeled 'Class name:'
			*/
			public static final String TEXT_CLASS_NAME = "Class name:";
			/**
			* text field labeled 'Web project:'
			*/
			public static final String TEXT_WEB_PROJECT = "Web project:";
			/**
			* checkbox field labeled 'Use existing Listener class'
			*/
			public static final String CHB_USE_EXISTING_LISTENER_CLASS = "Use existing Listener class";
			}
		public static class PluginDevelopmentComponentDefinition {
			/**
			* represents item : Plug-in Development->Component Definition
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Component Definition";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class GeneralUntitledTextFile {
			/**
			* represents item : General->Untitled Text File
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Untitled Text File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class EclipseModelingFrameworkEMFProject {
			/**
			* represents item : Eclipse Modeling Framework->EMF Project
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "EMF Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Eclipse Modeling Framework");
					return l;
				}
			};
			/**
			* text field labeled 'Project name:'
			*/
			public static final String TEXT_PROJECT_NAME = "Project name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Use default location'
			*/
			public static final String CHB_USE_DEFAULT_LOCATION = "Use default location";
			}
		public static class EclipseModelingFrameworkMappingEcoretoEcoreMapping {
			/**
			* represents item : Eclipse Modeling Framework->Mapping->Ecore to Ecore Mapping
			*/
			public static final INewObject LABEL = new INewObject() {
				public String getName() { return "Ecore to Ecore Mapping";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Eclipse Modeling Framework");
					l.add("Mapping");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}

		}

	public static class Import {
		public static class CVSProjectsfromCVS {
			/**
			* represents item : CVS->Projects from CVS
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Projects from CVS";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("CVS");
					return l;
				}
			};
			/**
			* text field labeled 'User:'
			*/
			public static final String TEXT_USER = "User:";
			/**
			* text field labeled 'Connection type:'
			*/
			public static final String TEXT_CONNECTION_TYPE = "Connection type:";
			/**
			* text field labeled 'Repository path:'
			*/
			public static final String TEXT_REPOSITORY_PATH = "Repository path:";
			/**
			* text field labeled 'Password:'
			*/
			public static final String TEXT_PASSWORD = "Password:";
			/**
			* text field labeled 'Host:'
			*/
			public static final String TEXT_HOST = "Host:";
			/**
			* checkbox field labeled 'Save password (could trigger secure storage login)'
			*/
			public static final String CHB_SAVE_PASSWORD_COULD_TRIGGER_SECURE_STORAGE_LOGIN = "Save password (could trigger secure storage login)";
			}
		public static class EJBEJBJARfile {
			/**
			* represents item : EJB->EJB JAR file
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "EJB JAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("EJB");
					return l;
				}
			};
			/**
			* text field labeled 'EJB Jar file:'
			*/
			public static final String TEXT_EJB_JAR_FILE = "EJB Jar file:";
			/**
			* text field labeled 'EJB project:'
			*/
			public static final String TEXT_EJB_PROJECT = "EJB project:";
			/**
			* text field labeled 'EAR project name:'
			*/
			public static final String TEXT_EAR_PROJECT_NAME = "EAR project name:";
			/**
			* text field labeled 'Target runtime:'
			*/
			public static final String TEXT_TARGET_RUNTIME = "Target runtime:";
			/**
			* checkbox field labeled 'Add project to an EAR'
			*/
			public static final String CHB_ADD_PROJECT_TO_AN_EAR = "Add project to an EAR";
			}
		public static class WebWARfile {
			/**
			* represents item : Web->WAR file
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "WAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'EAR project name:'
			*/
			public static final String TEXT_EAR_PROJECT_NAME = "EAR project name:";
			/**
			* text field labeled 'Target runtime:'
			*/
			public static final String TEXT_TARGET_RUNTIME = "Target runtime:";
			/**
			* text field labeled 'WAR file:'
			*/
			public static final String TEXT_WAR_FILE = "WAR file:";
			/**
			* text field labeled 'Web project:'
			*/
			public static final String TEXT_WEB_PROJECT = "Web project:";
			/**
			* checkbox field labeled 'Add project to an EAR'
			*/
			public static final String CHB_ADD_PROJECT_TO_AN_EAR = "Add project to an EAR";
			}
		public static class WebservicesWSIL {
			/**
			* represents item : Web services->WSIL
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "WSIL";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web services");
					return l;
				}
			};
			/**
			* text field labeled 'WSIL URI:'
			*/
			public static final String TEXT_WSIL_URI = "WSIL URI:";
			/**
			* text field labeled 'WSDL:'
			*/
			public static final String TEXT_WSDL = "WSDL:";
			}
		public static class RunDebugBreakpoints {
			/**
			* represents item : Run/Debug->Breakpoints
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Breakpoints";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					return l;
				}
			};
			/**
			* text field labeled 'From file:'
			*/
			public static final String TEXT_FROM_FILE = "From file:";
			/**
			* checkbox field labeled 'Update existing breakpoints'
			*/
			public static final String CHB_UPDATE_EXISTING_BREAKPOINTS = "Update existing breakpoints";
			/**
			* checkbox field labeled 'Create breakpoint working sets'
			*/
			public static final String CHB_CREATE_BREAKPOINT_WORKING_SETS = "Create breakpoint working sets";
			}
		public static class PluginDevelopmentPluginsandFragments {
			/**
			* represents item : Plug-in Development->Plug-ins and Fragments
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Plug-ins and Fragments";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			}
		public static class WebservicesWebService {
			/**
			* represents item : Web services->Web Service
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Web Service";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web services");
					return l;
				}
			};
			/**
			* checkbox field labeled 'Launch the Web Services Explorer to find a Web service from a UDDI Registry'
			*/
			public static final String CHB_LAUNCH_THE_WEB_SERVICES_EXPLORER_TO_FIND_A_WEB_SERVICE_FROM_A_UDDI_REGISTRY = "Launch the Web Services Explorer to find a Web service from a UDDI Registry";
			/**
			* checkbox field labeled 'Launch the Web Services Explorer to find a Web service from the Unit Test UDDI Registry'
			*/
			public static final String CHB_LAUNCH_THE_WEB_SERVICES_EXPLORER_TO_FIND_A_WEB_SERVICE_FROM_THE_UNIT_TEST_UDDI_REGISTRY = "Launch the Web Services Explorer to find a Web service from the Unit Test UDDI Registry";
			}
		public static class RunDebugLaunchConfigurations {
			/**
			* represents item : Run/Debug->Launch Configurations
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Launch Configurations";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					return l;
				}
			};
			/**
			* text field labeled 'From Directory:'
			*/
			public static final String TEXT_FROM_DIRECTORY = "From Directory:";
			/**
			* checkbox field labeled 'Overwrite existing launch configurations without warning.'
			*/
			public static final String CHB_OVERWRITE_EXISTING_LAUNCH_CONFIGURATIONS_WITHOUT_WARNING = "Overwrite existing launch configurations without warning.";
			}
		public static class GeneralPreferences {
			/**
			* represents item : General->Preferences
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Preferences";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'Description:'
			*/
			public static final String TEXT_DESCRIPTION = "Description:";
			/**
			* text field labeled 'From preference file:'
			*/
			public static final String TEXT_FROM_PREFERENCE_FILE = "From preference file:";
			}
		public static class OtherUnknowntagstemplates {
			/**
			* represents item : Other->Unknown tags templates
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Unknown tags templates";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Other");
					return l;
				}
			};
			}
		public static class GuvnorResourcesfromGuvnor {
			/**
			* represents item : Guvnor->Resources from Guvnor
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Resources from Guvnor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Guvnor");
					return l;
				}
			};
			}
		public static class JavaEEAppClientJARfile {
			/**
			* represents item : Java EE->App Client JAR file
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "App Client JAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'Application Client file:'
			*/
			public static final String TEXT_APPLICATION_CLIENT_FILE = "Application Client file:";
			/**
			* text field labeled 'EAR project name:'
			*/
			public static final String TEXT_EAR_PROJECT_NAME = "EAR project name:";
			/**
			* text field labeled 'Target runtime:'
			*/
			public static final String TEXT_TARGET_RUNTIME = "Target runtime:";
			/**
			* text field labeled 'Application Client project:'
			*/
			public static final String TEXT_APPLICATION_CLIENT_PROJECT = "Application Client project:";
			/**
			* checkbox field labeled 'Add project to an EAR'
			*/
			public static final String CHB_ADD_PROJECT_TO_AN_EAR = "Add project to an EAR";
			}
		public static class TeamTeamProjectSet {
			/**
			* represents item : Team->Team Project Set
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Team Project Set";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* checkbox field labeled 'Run the import in the background'
			*/
			public static final String CHB_RUN_THE_IMPORT_IN_THE_BACKGROUND = "Run the import in the background";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class JavaEEJ2EEUtilityJar {
			/**
			* represents item : Java EE->J2EE Utility Jar
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "J2EE Utility Jar";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'EAR Project:'
			*/
			public static final String TEXT_EAR_PROJECT = "EAR Project:";
			/**
			* text field labeled 'Module Root Location:'
			*/
			public static final String TEXT_MODULE_ROOT_LOCATION = "Module Root Location:";
			/**
			* checkbox field labeled 'Override Project Root (Specify location below)'
			*/
			public static final String CHB_OVERRIDE_PROJECT_ROOT_SPECIFY_LOCATION_BELOW = "Override Project Root (Specify location below)";
			}
		public static class JavaEERARfile {
			/**
			* represents item : Java EE->RAR file
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "RAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'EAR project name:'
			*/
			public static final String TEXT_EAR_PROJECT_NAME = "EAR project name:";
			/**
			* text field labeled 'Target runtime:'
			*/
			public static final String TEXT_TARGET_RUNTIME = "Target runtime:";
			/**
			* text field labeled 'Connector file:'
			*/
			public static final String TEXT_CONNECTOR_FILE = "Connector file:";
			/**
			* text field labeled 'Connector module:'
			*/
			public static final String TEXT_CONNECTOR_MODULE = "Connector module:";
			/**
			* checkbox field labeled 'Add project to an EAR'
			*/
			public static final String CHB_ADD_PROJECT_TO_AN_EAR = "Add project to an EAR";
			}
		public static class OtherJSFProject {
			/**
			* represents item : Other->JSF Project
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "JSF Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Other");
					return l;
				}
			};
			/**
			* text field labeled 'web.xml Location*'
			*/
			public static final String TEXT_WEBXML_LOCATION = "web.xml Location*";
			/**
			* text field labeled 'Project Name*'
			*/
			public static final String TEXT_PROJECT_NAME = "Project Name*";
			/**
			* checkbox field labeled 'Create Eclipse project in Workspace  '
			*/
			public static final String CHB_CREATE_ECLIPSE_PROJECT_IN_WORKSPACE__ = "Create Eclipse project in Workspace  ";
			}
		public static class OtherStrutsProject {
			/**
			* represents item : Other->Struts Project
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Struts Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Other");
					return l;
				}
			};
			/**
			* text field labeled 'web.xml Location*'
			*/
			public static final String TEXT_WEBXML_LOCATION = "web.xml Location*";
			/**
			* text field labeled 'Project Name*'
			*/
			public static final String TEXT_PROJECT_NAME = "Project Name*";
			/**
			* checkbox field labeled 'Create Eclipse project in Workspace  '
			*/
			public static final String CHB_CREATE_ECLIPSE_PROJECT_IN_WORKSPACE__ = "Create Eclipse project in Workspace  ";
			}
		public static class GeneralFileSystem {
			/**
			* represents item : General->File System
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "File System";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'From directory:'
			*/
			public static final String TEXT_FROM_DIRECTORY = "From directory:";
			/**
			* text field labeled 'Into folder:'
			*/
			public static final String TEXT_INTO_FOLDER = "Into folder:";
			/**
			* checkbox field labeled 'Overwrite existing resources without warning'
			*/
			public static final String CHB_OVERWRITE_EXISTING_RESOURCES_WITHOUT_WARNING = "Overwrite existing resources without warning";
			}
		public static class PluginDevelopmentFeatures {
			/**
			* represents item : Plug-in Development->Features
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Features";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'Select an import source:'
			*/
			public static final String TEXT_SELECT_AN_IMPORT_SOURCE = "Select an import source:";
			/**
			* text field labeled 'Feature Location:'
			*/
			public static final String TEXT_FEATURE_LOCATION = "Feature Location:";
			/**
			* checkbox field labeled 'Import as binary projects'
			*/
			public static final String CHB_IMPORT_AS_BINARY_PROJECTS = "Import as binary projects";
			/**
			* checkbox field labeled 'Choose from features in the target platform'
			*/
			public static final String CHB_CHOOSE_FROM_FEATURES_IN_THE_TARGET_PLATFORM = "Choose from features in the target platform";
			}
		public static class GeneralExistingProjectsintoWorkspace {
			/**
			* represents item : General->Existing Projects into Workspace
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Existing Projects into Workspace";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'Projects:'
			*/
			public static final String TEXT_PROJECTS = "Projects:";
			/**
			* text field labeled 'Working sets:'
			*/
			public static final String TEXT_WORKING_SETS = "Working sets:";
			/**
			* checkbox field labeled 'Copy projects into workspace'
			*/
			public static final String CHB_COPY_PROJECTS_INTO_WORKSPACE = "Copy projects into workspace";
			/**
			* checkbox field labeled 'Add project to working sets'
			*/
			public static final String CHB_ADD_PROJECT_TO_WORKING_SETS = "Add project to working sets";
			}
		public static class XMLXMLCatalog {
			/**
			* represents item : XML->XML Catalog
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "XML Catalog";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			}
		public static class GeneralArchiveFile {
			/**
			* represents item : General->Archive File
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Archive File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'From archive file:'
			*/
			public static final String TEXT_FROM_ARCHIVE_FILE = "From archive file:";
			/**
			* text field labeled 'Into folder:'
			*/
			public static final String TEXT_INTO_FOLDER = "Into folder:";
			/**
			* checkbox field labeled 'Overwrite existing resources without warning'
			*/
			public static final String CHB_OVERWRITE_EXISTING_RESOURCES_WITHOUT_WARNING = "Overwrite existing resources without warning";
			}
		public static class SVNProjectfromSVN {
			/**
			* represents item : SVN->Project from SVN
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Project from SVN";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("SVN");
					return l;
				}
			};
			/**
			* text field labeled 'User:'
			*/
			public static final String TEXT_USER = "User:";
			/**
			* text field labeled 'Show Credentials For:'
			*/
			public static final String TEXT_SHOW_CREDENTIALS_FOR = "Show Credentials For:";
			/**
			* text field labeled 'URL:'
			*/
			public static final String TEXT_URL = "URL:";
			/**
			* text field labeled 'Password:'
			*/
			public static final String TEXT_PASSWORD = "Password:";
			/**
			* checkbox field labeled 'Save password'
			*/
			public static final String CHB_SAVE_PASSWORD = "Save password";
			/**
			* checkbox field labeled 'Validate Repository Location on finish'
			*/
			public static final String CHB_VALIDATE_REPOSITORY_LOCATION_ON_FINISH = "Validate Repository Location on finish";
			}
		public static class OtherJSFProjectFromwar {
			/**
			* represents item : Other->JSF Project From *.war
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "JSF Project From *.war";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Other");
					return l;
				}
			};
			/**
			* text field labeled 'Location*'
			*/
			public static final String TEXT_LOCATION = "Location*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			/**
			* text field labeled 'Context Path*'
			*/
			public static final String TEXT_CONTEXT_PATH = "Context Path*";
			/**
			* text field labeled 'Runtime*'
			*/
			public static final String TEXT_RUNTIME = "Runtime*";
			/**
			* text field labeled '*.war Location:*'
			*/
			public static final String TEXT_WAR_LOCATION = "*.war Location:*";
			/**
			* checkbox field labeled 'Use Default Path'
			*/
			public static final String CHB_USE_DEFAULT_PATH = "Use Default Path";
			}
		public static class OtherStrutsProjectfromwar {
			/**
			* represents item : Other->Struts Project from *.war
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "Struts Project from *.war";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Other");
					return l;
				}
			};
			/**
			* text field labeled 'Location*'
			*/
			public static final String TEXT_LOCATION = "Location*";
			/**
			* text field labeled 'Name*'
			*/
			public static final String TEXT_NAME = "Name*";
			/**
			* text field labeled 'Context Path*'
			*/
			public static final String TEXT_CONTEXT_PATH = "Context Path*";
			/**
			* text field labeled 'Runtime*'
			*/
			public static final String TEXT_RUNTIME = "Runtime*";
			/**
			* text field labeled '*.war Location:*'
			*/
			public static final String TEXT_WAR_LOCATION = "*.war Location:*";
			/**
			* checkbox field labeled 'Use Default Path'
			*/
			public static final String CHB_USE_DEFAULT_PATH = "Use Default Path";
			}
		public static class JavaEEEARfile {
			/**
			* represents item : Java EE->EAR file
			*/
			public static final IImport LABEL = new IImport() {
				public String getName() { return "EAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'Target runtime:'
			*/
			public static final String TEXT_TARGET_RUNTIME = "Target runtime:";
			/**
			* text field labeled 'EAR project:'
			*/
			public static final String TEXT_EAR_PROJECT = "EAR project:";
			/**
			* text field labeled 'EAR file:'
			*/
			public static final String TEXT_EAR_FILE = "EAR file:";
			}

		}

	public static class Export {
		public static class JavaJARfile {
			/**
			* represents item : Java->JAR file
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "JAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			/**
			* text field labeled 'Options:'
			*/
			public static final String TEXT_OPTIONS = "Options:";
			/**
			* text field labeled 'Select the export destination:'
			*/
			public static final String TEXT_SELECT_THE_EXPORT_DESTINATION = "Select the export destination:";
			/**
			* text field labeled 'Select the resources to export:'
			*/
			public static final String TEXT_SELECT_THE_RESOURCES_TO_EXPORT = "Select the resources to export:";
			/**
			* text field labeled 'JAR file:'
			*/
			public static final String TEXT_JAR_FILE = "JAR file:";
			/**
			* checkbox field labeled 'Export generated class files and resources'
			*/
			public static final String CHB_EXPORT_GENERATED_CLASS_FILES_AND_RESOURCES = "Export generated class files and resources";
			/**
			* checkbox field labeled 'Export Java source files and resources'
			*/
			public static final String CHB_EXPORT_JAVA_SOURCE_FILES_AND_RESOURCES = "Export Java source files and resources";
			/**
			* checkbox field labeled 'Overwrite existing files without warning'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILES_WITHOUT_WARNING = "Overwrite existing files without warning";
			/**
			* checkbox field labeled 'Add directory entries'
			*/
			public static final String CHB_ADD_DIRECTORY_ENTRIES = "Add directory entries";
			/**
			* checkbox field labeled 'Export refactorings for checked projects.'
			*/
			public static final String CHB_EXPORT_REFACTORINGS_FOR_CHECKED_PROJECTS = "Export refactorings for checked projects.";
			/**
			* checkbox field labeled 'Compress the contents of the JAR file'
			*/
			public static final String CHB_COMPRESS_THE_CONTENTS_OF_THE_JAR_FILE = "Compress the contents of the JAR file";
			/**
			* checkbox field labeled 'Export all output folders for checked projects'
			*/
			public static final String CHB_EXPORT_ALL_OUTPUT_FOLDERS_FOR_CHECKED_PROJECTS = "Export all output folders for checked projects";
			}
		public static class PluginDevelopmentDeployablepluginsandfragments {
			/**
			* represents item : Plug-in Development->Deployable plug-ins and fragments
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Deployable plug-ins and fragments";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'Available Plug-ins and Fragments:'
			*/
			public static final String TEXT_AVAILABLE_PLUGINS_AND_FRAGMENTS = "Available Plug-ins and Fragments:";
			}
		public static class EJBEJBJARfile {
			/**
			* represents item : EJB->EJB JAR file
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "EJB JAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("EJB");
					return l;
				}
			};
			/**
			* text field labeled 'EJB project:'
			*/
			public static final String TEXT_EJB_PROJECT = "EJB project:";
			/**
			* text field labeled 'Destination:'
			*/
			public static final String TEXT_DESTINATION = "Destination:";
			/**
			* checkbox field labeled 'Optimize for a specific server runtime'
			*/
			public static final String CHB_OPTIMIZE_FOR_A_SPECIFIC_SERVER_RUNTIME = "Optimize for a specific server runtime";
			/**
			* checkbox field labeled 'Overwrite existing file'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILE = "Overwrite existing file";
			/**
			* checkbox field labeled 'Export source files'
			*/
			public static final String CHB_EXPORT_SOURCE_FILES = "Export source files";
			}
		public static class JavaEERARfile {
			/**
			* represents item : Java EE->RAR file
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "RAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'Connector project:'
			*/
			public static final String TEXT_CONNECTOR_PROJECT = "Connector project:";
			/**
			* text field labeled 'Destination:'
			*/
			public static final String TEXT_DESTINATION = "Destination:";
			/**
			* checkbox field labeled 'Optimize for a specific server runtime'
			*/
			public static final String CHB_OPTIMIZE_FOR_A_SPECIFIC_SERVER_RUNTIME = "Optimize for a specific server runtime";
			/**
			* checkbox field labeled 'Overwrite existing file'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILE = "Overwrite existing file";
			/**
			* checkbox field labeled 'Export source files'
			*/
			public static final String CHB_EXPORT_SOURCE_FILES = "Export source files";
			}
		public static class JavaEEModuleArchive {
			/**
			* represents item : Java EE->Module Archive
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Module Archive";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* checkbox field labeled 'Overwrite existing file'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILE = "Overwrite existing file";
			}
		public static class ESBESBFile {
			/**
			* represents item : ESB->ESB File
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "ESB File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("ESB");
					return l;
				}
			};
			/**
			* text field labeled 'ESB Project:'
			*/
			public static final String TEXT_ESB_PROJECT = "ESB Project:";
			/**
			* text field labeled 'Destination:'
			*/
			public static final String TEXT_DESTINATION = "Destination:";
			/**
			* checkbox field labeled 'Optimize for a specific server runtime'
			*/
			public static final String CHB_OPTIMIZE_FOR_A_SPECIFIC_SERVER_RUNTIME = "Optimize for a specific server runtime";
			/**
			* checkbox field labeled 'Overwrite existing file'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILE = "Overwrite existing file";
			/**
			* checkbox field labeled 'Export source files'
			*/
			public static final String CHB_EXPORT_SOURCE_FILES = "Export source files";
			}
		public static class WebWARfile {
			/**
			* represents item : Web->WAR file
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "WAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			/**
			* text field labeled 'Destination:'
			*/
			public static final String TEXT_DESTINATION = "Destination:";
			/**
			* text field labeled 'Web project:'
			*/
			public static final String TEXT_WEB_PROJECT = "Web project:";
			/**
			* checkbox field labeled 'Optimize for a specific server runtime'
			*/
			public static final String CHB_OPTIMIZE_FOR_A_SPECIFIC_SERVER_RUNTIME = "Optimize for a specific server runtime";
			/**
			* checkbox field labeled 'Overwrite existing file'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILE = "Overwrite existing file";
			/**
			* checkbox field labeled 'Export source files'
			*/
			public static final String CHB_EXPORT_SOURCE_FILES = "Export source files";
			}
		public static class PluginDevelopmentEclipseproduct {
			/**
			* represents item : Plug-in Development->Eclipse product
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Eclipse product";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'Root directory:'
			*/
			public static final String TEXT_ROOT_DIRECTORY = "Root directory:";
			/**
			* text field labeled 'Configuration:'
			*/
			public static final String TEXT_CONFIGURATION = "Configuration:";
			/**
			* checkbox field labeled 'Generate metadata repository'
			*/
			public static final String CHB_GENERATE_METADATA_REPOSITORY = "Generate metadata repository";
			/**
			* checkbox field labeled 'Export source:'
			*/
			public static final String CHB_EXPORT_SOURCE = "Export source:";
			/**
			* checkbox field labeled 'Allow for binary cycles in target platform'
			*/
			public static final String CHB_ALLOW_FOR_BINARY_CYCLES_IN_TARGET_PLATFORM = "Allow for binary cycles in target platform";
			/**
			* checkbox field labeled 'Synchronize before exporting'
			*/
			public static final String CHB_SYNCHRONIZE_BEFORE_EXPORTING = "Synchronize before exporting";
			}
		public static class GeneralFileSystem {
			/**
			* represents item : General->File System
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "File System";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'To directory:'
			*/
			public static final String TEXT_TO_DIRECTORY = "To directory:";
			/**
			* checkbox field labeled 'Overwrite existing files without warning'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILES_WITHOUT_WARNING = "Overwrite existing files without warning";
			}
		public static class XMLXMLCatalog {
			/**
			* represents item : XML->XML Catalog
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "XML Catalog";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			/**
			* text field labeled 'File name:'
			*/
			public static final String TEXT_FILE_NAME = "File name:";
			/**
			* text field labeled 'Enter or select the parent folder:'
			*/
			public static final String TEXT_ENTER_OR_SELECT_THE_PARENT_FOLDER = "Enter or select the parent folder:";
			}
		public static class RunDebugBreakpoints {
			/**
			* represents item : Run/Debug->Breakpoints
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Breakpoints";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					return l;
				}
			};
			/**
			* text field labeled 'To file:'
			*/
			public static final String TEXT_TO_FILE = "To file:";
			/**
			* text field labeled 'Breakpoints:'
			*/
			public static final String TEXT_BREAKPOINTS = "Breakpoints:";
			/**
			* checkbox field labeled 'Overwrite existing file without warning'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILE_WITHOUT_WARNING = "Overwrite existing file without warning";
			}
		public static class GeneralArchiveFile {
			/**
			* represents item : General->Archive File
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Archive File";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'To archive file:'
			*/
			public static final String TEXT_TO_ARCHIVE_FILE = "To archive file:";
			/**
			* checkbox field labeled 'Compress the contents of the file'
			*/
			public static final String CHB_COMPRESS_THE_CONTENTS_OF_THE_FILE = "Compress the contents of the file";
			}
		public static class WebServicesWebService {
			/**
			* represents item : Web Services->Web Service
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Web Service";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			/**
			* checkbox field labeled 'Launch the Web Services Explorer to publish this Web service to the Unit Test UDDI Registry'
			*/
			public static final String CHB_LAUNCH_THE_WEB_SERVICES_EXPLORER_TO_PUBLISH_THIS_WEB_SERVICE_TO_THE_UNIT_TEST_UDDI_REGISTRY = "Launch the Web Services Explorer to publish this Web service to the Unit Test UDDI Registry";
			/**
			* checkbox field labeled 'Launch the Web Services Explorer to publish this Web service to a UDDI Registry'
			*/
			public static final String CHB_LAUNCH_THE_WEB_SERVICES_EXPLORER_TO_PUBLISH_THIS_WEB_SERVICE_TO_A_UDDI_REGISTRY = "Launch the Web Services Explorer to publish this Web service to a UDDI Registry";
			}
		public static class GeneralAntBuildfiles {
			/**
			* represents item : General->Ant Buildfiles
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Ant Buildfiles";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'Name for Ant buildfile:'
			*/
			public static final String TEXT_NAME_FOR_ANT_BUILDFILE = "Name for Ant buildfile:";
			/**
			* text field labeled 'JUnit output directory:'
			*/
			public static final String TEXT_JUNIT_OUTPUT_DIRECTORY = "JUnit output directory:";
			/**
			* text field labeled 'Select the projects to use to generate the Ant buildfiles:'
			*/
			public static final String TEXT_SELECT_THE_PROJECTS_TO_USE_TO_GENERATE_THE_ANT_BUILDFILES = "Select the projects to use to generate the Ant buildfiles:";
			/**
			* checkbox field labeled 'Create target to compile project using Eclipse compiler'
			*/
			public static final String CHB_CREATE_TARGET_TO_COMPILE_PROJECT_USING_ECLIPSE_COMPILER = "Create target to compile project using Eclipse compiler";
			/**
			* checkbox field labeled 'Check projects for Ant compatibility'
			*/
			public static final String CHB_CHECK_PROJECTS_FOR_ANT_COMPATIBILITY = "Check projects for Ant compatibility";
			}
		public static class JavaJavadoc {
			/**
			* represents item : Java->Javadoc
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Javadoc";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			/**
			* text field labeled 'Doclet name:'
			*/
			public static final String TEXT_DOCLET_NAME = "Doclet name:";
			/**
			* text field labeled 'Doclet class path:'
			*/
			public static final String TEXT_DOCLET_CLASS_PATH = "Doclet class path:";
			/**
			* text field labeled 'Create Javadoc for members with visibility:'
			*/
			public static final String TEXT_CREATE_JAVADOC_FOR_MEMBERS_WITH_VISIBILITY = "Create Javadoc for members with visibility:";
			/**
			* text field labeled 'Javadoc command:'
			*/
			public static final String TEXT_JAVADOC_COMMAND = "Javadoc command:";
			/**
			* text field labeled 'Destination:'
			*/
			public static final String TEXT_DESTINATION = "Destination:";
			/**
			* text field labeled 'Select types for which Javadoc will be generated:'
			*/
			public static final String TEXT_SELECT_TYPES_FOR_WHICH_JAVADOC_WILL_BE_GENERATED = "Select types for which Javadoc will be generated:";
			}
		public static class RunDebugLaunchConfigurations {
			/**
			* represents item : Run/Debug->Launch Configurations
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Launch Configurations";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					return l;
				}
			};
			/**
			* text field labeled 'Launch Configurations:'
			*/
			public static final String TEXT_LAUNCH_CONFIGURATIONS = "Launch Configurations:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			/**
			* checkbox field labeled 'Overwrite existing file(s) without warning'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILES_WITHOUT_WARNING = "Overwrite existing file(s) without warning";
			}
		public static class GeneralPreferences {
			/**
			* represents item : General->Preferences
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Preferences";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			/**
			* text field labeled 'Description:'
			*/
			public static final String TEXT_DESCRIPTION = "Description:";
			/**
			* text field labeled 'To preference file:'
			*/
			public static final String TEXT_TO_PREFERENCE_FILE = "To preference file:";
			/**
			* checkbox field labeled 'Overwrite existing files without warning'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILES_WITHOUT_WARNING = "Overwrite existing files without warning";
			}
		public static class OtherUnknowntagstemplates {
			/**
			* represents item : Other->Unknown tags templates
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Unknown tags templates";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Other");
					return l;
				}
			};
			}
		public static class BPMNBPMNtojPDL {
			/**
			* represents item : BPMN->BPMN to jPDL
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "BPMN to jPDL";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("BPMN");
					return l;
				}
			};
			}
		public static class PluginDevelopmentDeployablefeatures {
			/**
			* represents item : Plug-in Development->Deployable features
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Deployable features";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			/**
			* text field labeled 'Available Features:'
			*/
			public static final String TEXT_AVAILABLE_FEATURES = "Available Features:";
			}
		public static class JavaEEAppClientJARfile {
			/**
			* represents item : Java EE->App Client JAR file
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "App Client JAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'Destination:'
			*/
			public static final String TEXT_DESTINATION = "Destination:";
			/**
			* text field labeled 'Application Client project:'
			*/
			public static final String TEXT_APPLICATION_CLIENT_PROJECT = "Application Client project:";
			/**
			* checkbox field labeled 'Optimize for a specific server runtime'
			*/
			public static final String CHB_OPTIMIZE_FOR_A_SPECIFIC_SERVER_RUNTIME = "Optimize for a specific server runtime";
			/**
			* checkbox field labeled 'Overwrite existing file'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILE = "Overwrite existing file";
			/**
			* checkbox field labeled 'Export source files'
			*/
			public static final String CHB_EXPORT_SOURCE_FILES = "Export source files";
			}
		public static class TeamTeamProjectSet {
			/**
			* represents item : Team->Team Project Set
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Team Project Set";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					return l;
				}
			};
			/**
			* text field labeled 'Select the working sets to include in the Team Project Set:'
			*/
			public static final String TEXT_SELECT_THE_WORKING_SETS_TO_INCLUDE_IN_THE_TEAM_PROJECT_SET = "Select the working sets to include in the Team Project Set:";
			/**
			* text field labeled 'Select the projects to include in the Team Project Set:'
			*/
			public static final String TEXT_SELECT_THE_PROJECTS_TO_INCLUDE_IN_THE_TEAM_PROJECT_SET = "Select the projects to include in the Team Project Set:";
			/**
			* checkbox field labeled 'Export working sets'
			*/
			public static final String CHB_EXPORT_WORKING_SETS = "Export working sets";
			}
		public static class JavaRunnableJARfile {
			/**
			* represents item : Java->Runnable JAR file
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "Runnable JAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			/**
			* text field labeled 'ANT script location:'
			*/
			public static final String TEXT_ANT_SCRIPT_LOCATION = "ANT script location:";
			/**
			* text field labeled 'Export destination:'
			*/
			public static final String TEXT_EXPORT_DESTINATION = "Export destination:";
			/**
			* text field labeled 'Library handling:'
			*/
			public static final String TEXT_LIBRARY_HANDLING = "Library handling:";
			/**
			* text field labeled 'Launch configuration:'
			*/
			public static final String TEXT_LAUNCH_CONFIGURATION = "Launch configuration:";
			/**
			* checkbox field labeled 'Save as ANT script'
			*/
			public static final String CHB_SAVE_AS_ANT_SCRIPT = "Save as ANT script";
			}
		public static class JavaEEEARfile {
			/**
			* represents item : Java EE->EAR file
			*/
			public static final IExport LABEL = new IExport() {
				public String getName() { return "EAR file";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java EE");
					return l;
				}
			};
			/**
			* text field labeled 'Destination:'
			*/
			public static final String TEXT_DESTINATION = "Destination:";
			/**
			* text field labeled 'EAR project:'
			*/
			public static final String TEXT_EAR_PROJECT = "EAR project:";
			/**
			* checkbox field labeled 'Optimize for a specific server runtime'
			*/
			public static final String CHB_OPTIMIZE_FOR_A_SPECIFIC_SERVER_RUNTIME = "Optimize for a specific server runtime";
			/**
			* checkbox field labeled 'Overwrite existing file'
			*/
			public static final String CHB_OVERWRITE_EXISTING_FILE = "Overwrite existing file";
			/**
			* checkbox field labeled 'Export source files'
			*/
			public static final String CHB_EXPORT_SOURCE_FILES = "Export source files";
			}

		}

	public static class Preference {
		/**
		 * creates new action item instance from given path 
		 * @param path
		 * @return
		 */
		public static IPreference create(final String...path) {
			if (path.length<1) {
				throw new IllegalArgumentException("path must contain at least 1 item");
			}
			return new IPreference() {
		
				@Override
				public String getName() {
					// TODO Auto-generated method stub
					return path[path.length-1];
				}
		
				@Override
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					for (int i=0; i<path.length-1;i++) {
						l.add(path[i]);
					}
					return l;
				}
				
			};
		}
		public static class RunDebugViewManagement {
			/**
			* represents item : Run/Debug->View Management
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "View Management";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					return l;
				}
			};
			}
		public static class JavaInstalledJREsExecutionEnvironments {
			/**
			* represents item : Java->Installed JREs->Execution Environments
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Execution Environments";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Installed JREs");
					return l;
				}
			};
			}
		public static class GeneralWorkspaceBuildOrder {
			/**
			* represents item : General->Workspace->Build Order
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Build Order";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Workspace");
					return l;
				}
			};
			}
		public static class PluginDevelopmentAPIErrorsWarnings {
			/**
			* represents item : Plug-in Development->API Errors/Warnings
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "API Errors/Warnings";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentSQLResultsViewOptionsResultSetViewerOptions {
			/**
			* represents item : Data Management->SQL Development->SQL Results View Options->Result Set Viewer Options
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Result Set Viewer Options";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					l.add("SQL Results View Options");
					return l;
				}
			};
			}
		public static class TeamCVSWatchEdit {
			/**
			* represents item : Team->CVS->Watch/Edit
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Watch/Edit";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					l.add("CVS");
					return l;
				}
			};
			}
		public static class JavaCompiler {
			/**
			* represents item : Java->Compiler
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Compiler";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class GeneralNetworkConnections {
			/**
			* represents item : General->Network Connections
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Network Connections";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JavaEditorMarkOccurrences {
			/**
			* represents item : Java->Editor->Mark Occurrences
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Mark Occurrences";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Editor");
					return l;
				}
			};
			}
		public static class WebJSPFiles {
			/**
			* represents item : Web->JSP Files
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JSP Files";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			}
		public static class JBossToolsWebJSFProject {
			/**
			* represents item : JBoss Tools->Web->JSF->Project
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Project";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("JSF");
					return l;
				}
			};
			}
		public static class JavaJUnit {
			/**
			* represents item : Java->JUnit
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JUnit";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class WebServicesResourceManagement {
			/**
			* represents item : Web Services->Resource Management
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Resource Management";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class WebServicesJBossWSPreferences {
			/**
			* represents item : Web Services->JBossWS Preferences
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JBossWS Preferences";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class Team {
			/**
			* represents item : Team
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Team";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class DroolsInstalledDroolsRuntimes {
			/**
			* represents item : Drools->Installed Drools Runtimes
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Installed Drools Runtimes";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			}
		public static class Server {
			/**
			* represents item : Server
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Server";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class XMLXMLCatalog {
			/**
			* represents item : XML->XML Catalog
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XML Catalog";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			}
		public static class RunDebugLaunchingDefaultLaunchers {
			/**
			* represents item : Run/Debug->Launching->Default Launchers
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Default Launchers";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					l.add("Launching");
					return l;
				}
			};
			}
		public static class TeamFileContent {
			/**
			* represents item : Team->File Content
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "File Content";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					return l;
				}
			};
			}
		public static class GeneralWorkspaceLocalHistory {
			/**
			* represents item : General->Workspace->Local History
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Local History";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Workspace");
					return l;
				}
			};
			}
		public static class TeamCVS {
			/**
			* represents item : Team->CVS
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "CVS";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					return l;
				}
			};
			}
		public static class GeneralComparePatch {
			/**
			* represents item : General->Compare/Patch
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Compare/Patch";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class HQLeditor {
			/**
			* represents item : HQL editor
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "HQL editor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class GeneralWebBrowser {
			/**
			* represents item : General->Web Browser
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Web Browser";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JBossToolsJBossPortlet {
			/**
			* represents item : JBoss Tools->JBoss Portlet
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JBoss Portlet";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					return l;
				}
			};
			}
		public static class GeneralEditorsTextEditorsQuickDiff {
			/**
			* represents item : General->Editors->Text Editors->Quick Diff
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Quick Diff";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Editors");
					l.add("Text Editors");
					return l;
				}
			};
			}
		public static class Guvnor {
			/**
			* represents item : Guvnor
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Guvnor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class GeneralWorkspaceLinkedResources {
			/**
			* represents item : General->Workspace->Linked Resources
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Linked Resources";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Workspace");
					return l;
				}
			};
			}
		public static class XMLXSLJavaProcessors {
			/**
			* represents item : XML->XSL->Java Processors
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Java Processors";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					l.add("XSL");
					return l;
				}
			};
			}
		public static class PluginDevelopmentAPIBaselines {
			/**
			* represents item : Plug-in Development->API Baselines
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "API Baselines";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			}
		public static class Spring {
			/**
			* represents item : Spring
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Spring";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JavaEE {
			/**
			* represents item : Java EE
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Java EE";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentSQLQueryBuilder {
			/**
			* represents item : Data Management->SQL Development->SQL Query Builder
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "SQL Query Builder";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					return l;
				}
			};
			}
		public static class ServerProfilers {
			/**
			* represents item : Server->Profilers
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Profilers";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Server");
					return l;
				}
			};
			}
		public static class JBossToolsWebJSF {
			/**
			* represents item : JBoss Tools->Web->JSF
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JSF";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					return l;
				}
			};
			}
		public static class JBossToolsWebStrutsProjectStrutsSupport {
			/**
			* represents item : JBoss Tools->Web->Struts->Project->Struts Support
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Struts Support";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Struts");
					l.add("Project");
					return l;
				}
			};
			}
		public static class JBossToolsWebCDI {
			/**
			* represents item : JBoss Tools->Web->CDI
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "CDI";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					return l;
				}
			};
			}
		public static class GeneralEditorsTextEditorsLinkedMode {
			/**
			* represents item : General->Editors->Text Editors->Linked Mode
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Linked Mode";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Editors");
					l.add("Text Editors");
					return l;
				}
			};
			}
		public static class WebJavaServerFacesToolsViews {
			/**
			* represents item : Web->JavaServer Faces Tools->Views
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Views";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					l.add("JavaServer Faces Tools");
					return l;
				}
			};
			}
		public static class GeneralAppearanceLabelDecorations {
			/**
			* represents item : General->Appearance->Label Decorations
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Label Decorations";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Appearance");
					return l;
				}
			};
			}
		public static class WebJavaScriptValidatorJSDoc {
			/**
			* represents item : Web->JavaScript->Validator->JSDoc
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JSDoc";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					l.add("JavaScript");
					l.add("Validator");
					return l;
				}
			};
			}
		public static class HelpContent {
			/**
			* represents item : Help->Content
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Content";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Help");
					return l;
				}
			};
			}
		public static class TeamSVNPropertiesConfiguration {
			/**
			* represents item : Team->SVN->Properties Configuration
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Properties Configuration";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					l.add("SVN");
					return l;
				}
			};
			}
		public static class XMLXPathXPathTemplates {
			/**
			* represents item : XML->XPath->XPath Templates
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XPath Templates";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					l.add("XPath");
					return l;
				}
			};
			}
		public static class JBossToolsWebEditorsStrutsFlowDiagram {
			/**
			* represents item : JBoss Tools->Web->Editors->Struts Flow Diagram
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Struts Flow Diagram";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Editors");
					return l;
				}
			};
			}
		public static class SpringWebFlowSupport {
			/**
			* represents item : Spring->Web Flow Support
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Web Flow Support";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					return l;
				}
			};
			}
		public static class JavaDebug {
			/**
			* represents item : Java->Debug
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Debug";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class JavaEditorContentAssistAdvanced {
			/**
			* represents item : Java->Editor->Content Assist->Advanced
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Advanced";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Editor");
					l.add("Content Assist");
					return l;
				}
			};
			}
		public static class WebHTMLFiles {
			/**
			* represents item : Web->HTML Files
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "HTML Files";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			}
		public static class JBossToolsWebEditorsTilesDiagram {
			/**
			* represents item : JBoss Tools->Web->Editors->Tiles Diagram
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Tiles Diagram";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Editors");
					return l;
				}
			};
			}
		public static class JBossjBPMjBPM3JpdlTemplates {
			/**
			* represents item : JBoss jBPM->jBPM 3->Jpdl Templates
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Jpdl Templates";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss jBPM");
					l.add("jBPM 3");
					return l;
				}
			};
			}
		public static class XMLXPath {
			/**
			* represents item : XML->XPath
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XPath";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			}
		public static class XMLXMLFilesXMLOccurrences {
			/**
			* represents item : XML->XML Files->XML Occurrences
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XML Occurrences";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					l.add("XML Files");
					return l;
				}
			};
			}
		public static class JavaCodeStyle {
			/**
			* represents item : Java->Code Style
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Code Style";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class XML {
			/**
			* represents item : XML
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XML";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentSQLResultsViewOptions {
			/**
			* represents item : Data Management->SQL Development->SQL Results View Options
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "SQL Results View Options";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					return l;
				}
			};
			}
		public static class JavaEditorHovers {
			/**
			* represents item : Java->Editor->Hovers
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Hovers";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Editor");
					return l;
				}
			};
			}
		public static class GeneralStartupandShutdownWorkspaces {
			/**
			* represents item : General->Startup and Shutdown->Workspaces
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Workspaces";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Startup and Shutdown");
					return l;
				}
			};
			}
		public static class JBossToolsWebStrutsAutomation {
			/**
			* represents item : JBoss Tools->Web->Struts->Automation
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Automation";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Struts");
					return l;
				}
			};
			}
		public static class SpringBeansSupport {
			/**
			* represents item : Spring->Beans Support
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Beans Support";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					return l;
				}
			};
			}
		public static class Validation {
			/**
			* represents item : Validation
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Validation";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class General {
			/**
			* represents item : General
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "General";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JavaCompilerJavadoc {
			/**
			* represents item : Java->Compiler->Javadoc
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Javadoc";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Compiler");
					return l;
				}
			};
			}
		public static class XMLXMLFiles {
			/**
			* represents item : XML->XML Files
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XML Files";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			}
		public static class WebServicesWizardValidation {
			/**
			* represents item : Web Services->Wizard Validation
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Wizard Validation";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class GeneralEditorsTextEditorsSpelling {
			/**
			* represents item : General->Editors->Text Editors->Spelling
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Spelling";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Editors");
					l.add("Text Editors");
					return l;
				}
			};
			}
		public static class DroolsDroolsFlownodes {
			/**
			* represents item : Drools->Drools Flow nodes
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Drools Flow nodes";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Drools");
					return l;
				}
			};
			}
		public static class SpringBeansSupportXMLTemplates {
			/**
			* represents item : Spring->Beans Support->XML Templates
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XML Templates";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					l.add("Beans Support");
					return l;
				}
			};
			}
		public static class GeneralStartupandShutdown {
			/**
			* represents item : General->Startup and Shutdown
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Startup and Shutdown";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class AntRuntime {
			/**
			* represents item : Ant->Runtime
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Ant");
					return l;
				}
			};
			}
		public static class GeneralPerspectives {
			/**
			* represents item : General->Perspectives
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Perspectives";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentSQLResultsViewOptionsHistoryOptions {
			/**
			* represents item : Data Management->SQL Development->SQL Results View Options->History Options
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "History Options";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					l.add("SQL Results View Options");
					return l;
				}
			};
			}
		public static class WebServicesScenarioDefaults {
			/**
			* represents item : Web Services->Scenario Defaults
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Scenario Defaults";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class JavaAppearanceMembersSortOrder {
			/**
			* represents item : Java->Appearance->Members Sort Order
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Members Sort Order";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Appearance");
					return l;
				}
			};
			}
		public static class JavaInstalledJREs {
			/**
			* represents item : Java->Installed JREs
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Installed JREs";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class WebCSSFiles {
			/**
			* represents item : Web->CSS Files
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "CSS Files";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			}
		public static class JBossToolsWeb {
			/**
			* represents item : JBoss Tools->Web
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Web";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					return l;
				}
			};
			}
		public static class PluginDevelopmentTargetPlatform {
			/**
			* represents item : Plug-in Development->Target Platform
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Target Platform";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			}
		public static class JavaDebugDetailFormatters {
			/**
			* represents item : Java->Debug->Detail Formatters
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Detail Formatters";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Debug");
					return l;
				}
			};
			}
		public static class JBossToolsWebStrutsCustomization {
			/**
			* represents item : JBoss Tools->Web->Struts->Customization
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Customization";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Struts");
					return l;
				}
			};
			}
		public static class DataManagementConnectivityDatabaseConnectionProfile {
			/**
			* represents item : Data Management->Connectivity->Database Connection Profile
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Database Connection Profile";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("Connectivity");
					return l;
				}
			};
			}
		public static class WebServicesWSDLFiles {
			/**
			* represents item : Web Services->WSDL Files
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "WSDL Files";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class JBossToolsJBossESBRuntimes {
			/**
			* represents item : JBoss Tools->JBoss ESB Runtimes
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JBoss ESB Runtimes";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					return l;
				}
			};
			}
		public static class GeneralSecuritySecureStorage {
			/**
			* represents item : General->Security->Secure Storage
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Secure Storage";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Security");
					return l;
				}
			};
			}
		public static class TeamCVSSynchronizeCompare {
			/**
			* represents item : Team->CVS->Synchronize/Compare
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Synchronize/Compare";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					l.add("CVS");
					return l;
				}
			};
			}
		public static class GeneralAppearanceColorsandFonts {
			/**
			* represents item : General->Appearance->Colors and Fonts
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Colors and Fonts";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Appearance");
					return l;
				}
			};
			}
		public static class DataManagementConnectivityOpenDataAccess {
			/**
			* represents item : Data Management->Connectivity->Open Data Access
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Open Data Access";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("Connectivity");
					return l;
				}
			};
			}
		public static class JBossToolsWebEditorsVisualPageEditor {
			/**
			* represents item : JBoss Tools->Web->Editors->Visual Page Editor
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Visual Page Editor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Editors");
					return l;
				}
			};
			}
		public static class JavaEditorFolding {
			/**
			* represents item : Java->Editor->Folding
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Folding";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Editor");
					return l;
				}
			};
			}
		public static class InstallUpdate {
			/**
			* represents item : Install/Update
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Install/Update";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class TeamModels {
			/**
			* represents item : Team->Models
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Models";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					return l;
				}
			};
			}
		public static class AntEditorContentAssist {
			/**
			* represents item : Ant->Editor->Content Assist
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Content Assist";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Ant");
					l.add("Editor");
					return l;
				}
			};
			}
		public static class TeamSVNPasswordManagement {
			/**
			* represents item : Team->SVN->Password Management
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Password Management";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					l.add("SVN");
					return l;
				}
			};
			}
		public static class JBossjBPMRuntimeLocations {
			/**
			* represents item : JBoss jBPM->Runtime Locations
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Runtime Locations";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss jBPM");
					return l;
				}
			};
			}
		public static class WebJavaServerFacesToolsFacesConfigEditor {
			/**
			* represents item : Web->JavaServer Faces Tools->FacesConfig Editor
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "FacesConfig Editor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					l.add("JavaServer Faces Tools");
					return l;
				}
			};
			}
		public static class WebServicesProjectTopology {
			/**
			* represents item : Web Services->Project Topology
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Project Topology";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class XMLXMLSchemaFiles {
			/**
			* represents item : XML->XML Schema Files
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XML Schema Files";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			}
		public static class ProjectArchives {
			/**
			* represents item : Project Archives
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Project Archives";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class TeamCVSCommentTemplates {
			/**
			* represents item : Team->CVS->Comment Templates
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Comment Templates";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					l.add("CVS");
					return l;
				}
			};
			}
		public static class XMLDTDFiles {
			/**
			* represents item : XML->DTD Files
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "DTD Files";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			}
		public static class TeamSVNPerformance {
			/**
			* represents item : Team->SVN->Performance
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Performance";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					l.add("SVN");
					return l;
				}
			};
			}
		public static class JavaCompilerErrorsWarnings {
			/**
			* represents item : Java->Compiler->Errors/Warnings
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Errors/Warnings";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Compiler");
					return l;
				}
			};
			}
		public static class GeneralEditorsStructuredTextEditorsTaskTags {
			/**
			* represents item : General->Editors->Structured Text Editors->Task Tags
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Task Tags";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Editors");
					l.add("Structured Text Editors");
					return l;
				}
			};
			}
		public static class GeneralEditorsStructuredTextEditors {
			/**
			* represents item : General->Editors->Structured Text Editors
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Structured Text Editors";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Editors");
					return l;
				}
			};
			}
		public static class TeamSVNDiffViewer {
			/**
			* represents item : Team->SVN->Diff Viewer
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Diff Viewer";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					l.add("SVN");
					return l;
				}
			};
			}
		public static class PluginDevelopment {
			/**
			* represents item : Plug-in Development
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Plug-in Development";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JBossToolsWebSeam {
			/**
			* represents item : JBoss Tools->Web->Seam
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Seam";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					return l;
				}
			};
			}
		public static class RunDebugTCPIPMonitor {
			/**
			* represents item : Run/Debug->TCP/IP Monitor
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "TCP/IP Monitor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					return l;
				}
			};
			}
		public static class JavaDebugLogicalStructures {
			/**
			* represents item : Java->Debug->Logical Structures
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Logical Structures";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Debug");
					return l;
				}
			};
			}
		public static class WebJavaScriptIncludePath {
			/**
			* represents item : Web->JavaScript->Include Path
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Include Path";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					l.add("JavaScript");
					return l;
				}
			};
			}
		public static class JavaBuildPathUserLibraries {
			/**
			* represents item : Java->Build Path->User Libraries
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "User Libraries";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Build Path");
					return l;
				}
			};
			}
		public static class WebJSPFilesJSPOccurrences {
			/**
			* represents item : Web->JSP Files->JSP Occurrences
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JSP Occurrences";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					l.add("JSP Files");
					return l;
				}
			};
			}
		public static class TeamSVN {
			/**
			* represents item : Team->SVN
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "SVN";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentSQLEditor {
			/**
			* represents item : Data Management->SQL Development->SQL Editor
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "SQL Editor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					return l;
				}
			};
			}
		public static class RunDebugStringSubstitution {
			/**
			* represents item : Run/Debug->String Substitution
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "String Substitution";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentSQLEditorSyntaxColoring {
			/**
			* represents item : Data Management->SQL Development->SQL Editor->Syntax Coloring
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Syntax Coloring";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					l.add("SQL Editor");
					return l;
				}
			};
			}
		public static class GeneralSearch {
			/**
			* represents item : General->Search
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Search";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class AntEditorFormatter {
			/**
			* represents item : Ant->Editor->Formatter
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Formatter";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Ant");
					l.add("Editor");
					return l;
				}
			};
			}
		public static class JavaCompilerBuilding {
			/**
			* represents item : Java->Compiler->Building
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Building";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Compiler");
					return l;
				}
			};
			}
		public static class JBossjBPM {
			/**
			* represents item : JBoss jBPM
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JBoss jBPM";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class RunDebugLaunching {
			/**
			* represents item : Run/Debug->Launching
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Launching";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					return l;
				}
			};
			}
		public static class XDocletejbdoclet {
			/**
			* represents item : XDoclet->ejbdoclet
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "ejbdoclet";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XDoclet");
					return l;
				}
			};
			}
		public static class XDoclet {
			/**
			* represents item : XDoclet
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XDoclet";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class TeamCVSAnnotate {
			/**
			* represents item : Team->CVS->Annotate
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Annotate";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					l.add("CVS");
					return l;
				}
			};
			}
		public static class JBossToolsWebVerification {
			/**
			* represents item : JBoss Tools->Web->Verification
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Verification";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					return l;
				}
			};
			}
		public static class SpringAOPSupport {
			/**
			* represents item : Spring->AOP Support
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "AOP Support";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					return l;
				}
			};
			}
		public static class RunDebug {
			/**
			* represents item : Run/Debug
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Run/Debug";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class PluginDevelopmentCompilers {
			/**
			* represents item : Plug-in Development->Compilers
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Compilers";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			}
		public static class WebServicesServerandRuntime {
			/**
			* represents item : Web Services->Server and Runtime
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Server and Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class TestNG {
			/**
			* represents item : TestNG
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "TestNG";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JavaDebugPrimitiveDisplayOptions {
			/**
			* represents item : Java->Debug->Primitive Display Options
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Primitive Display Options";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Debug");
					return l;
				}
			};
			}
		public static class JBossToolsWebEditorsJSFFlowDiagram {
			/**
			* represents item : JBoss Tools->Web->Editors->JSF Flow Diagram
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JSF Flow Diagram";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Editors");
					return l;
				}
			};
			}
		public static class TeamCVSExtConnectionMethod {
			/**
			* represents item : Team->CVS->Ext Connection Method
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Ext Connection Method";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					l.add("CVS");
					return l;
				}
			};
			}
		public static class GeneralServicePolicies {
			/**
			* represents item : General->Service Policies
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Service Policies";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentSQLEditorCodeAssist {
			/**
			* represents item : Data Management->SQL Development->SQL Editor->Code Assist
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Code Assist";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					l.add("SQL Editor");
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentSQLEditorSQLFilesScrapbooks {
			/**
			* represents item : Data Management->SQL Development->SQL Editor->SQL Files/Scrapbooks
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "SQL Files/Scrapbooks";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					l.add("SQL Editor");
					return l;
				}
			};
			}
		public static class JavaDebugHeapWalking {
			/**
			* represents item : Java->Debug->Heap Walking
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Heap Walking";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Debug");
					return l;
				}
			};
			}
		public static class JBossToolsProjectExamples {
			/**
			* represents item : JBoss Tools->Project Examples
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Project Examples";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					return l;
				}
			};
			}
		public static class JavaAppearanceTypeFilters {
			/**
			* represents item : Java->Appearance->Type Filters
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Type Filters";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Appearance");
					return l;
				}
			};
			}
		public static class Drools {
			/**
			* represents item : Drools
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Drools";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class XMLXSL {
			/**
			* represents item : XML->XSL
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XSL";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XML");
					return l;
				}
			};
			}
		public static class GeneralNetworkConnectionsSSH2 {
			/**
			* represents item : General->Network Connections->SSH2
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "SSH2";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Network Connections");
					return l;
				}
			};
			}
		public static class GeneralEditorsFileAssociations {
			/**
			* represents item : General->Editors->File Associations
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "File Associations";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Editors");
					return l;
				}
			};
			}
		public static class RunDebugConsole {
			/**
			* represents item : Run/Debug->Console
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Console";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					return l;
				}
			};
			}
		public static class WebServicesPopupDialogSelection {
			/**
			* represents item : Web Services->Popup Dialog Selection
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Popup Dialog Selection";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class RunDebugExternalTools {
			/**
			* represents item : Run/Debug->External Tools
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "External Tools";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					return l;
				}
			};
			}
		public static class JavaCodeStyleCodeTemplates {
			/**
			* represents item : Java->Code Style->Code Templates
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Code Templates";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Code Style");
					return l;
				}
			};
			}
		public static class JBossjBPMjBPM3 {
			/**
			* represents item : JBoss jBPM->jBPM 3
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "jBPM 3";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss jBPM");
					return l;
				}
			};
			}
		public static class JBossToolsWebJSFJSFPages {
			/**
			* represents item : JBoss Tools->Web->JSF->JSF Pages
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JSF Pages";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("JSF");
					return l;
				}
			};
			}
		public static class JBossToolsWebStrutsAutomationPluginInsets {
			/**
			* represents item : JBoss Tools->Web->Struts->Automation->Plug-in Insets
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Plug-in Insets";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Struts");
					l.add("Automation");
					return l;
				}
			};
			}
		public static class XDocletwebdoclet {
			/**
			* represents item : XDoclet->webdoclet
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "webdoclet";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("XDoclet");
					return l;
				}
			};
			}
		public static class JavaEditorSaveActions {
			/**
			* represents item : Java->Editor->Save Actions
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Save Actions";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Editor");
					return l;
				}
			};
			}
		public static class JavaEditorContentAssistFavorites {
			/**
			* represents item : Java->Editor->Content Assist->Favorites
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Favorites";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Editor");
					l.add("Content Assist");
					return l;
				}
			};
			}
		public static class GeneralKeys {
			/**
			* represents item : General->Keys
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Keys";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentExecutionPlanViewOptions {
			/**
			* represents item : Data Management->SQL Development->Execution Plan View Options
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Execution Plan View Options";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentSchemaObjectEditorConfiguration {
			/**
			* represents item : Data Management->SQL Development->Schema Object Editor Configuration
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Schema Object Editor Configuration";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					return l;
				}
			};
			}
		public static class SpringBeansSupportNamespaceVersions {
			/**
			* represents item : Spring->Beans Support->Namespace Versions
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Namespace Versions";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Spring");
					l.add("Beans Support");
					return l;
				}
			};
			}
		public static class WebServices {
			/**
			* represents item : Web Services
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Web Services";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JBossToolsWebStrutsAutomationResourceInsets {
			/**
			* represents item : JBoss Tools->Web->Struts->Automation->Resource Insets
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Resource Insets";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Struts");
					l.add("Automation");
					return l;
				}
			};
			}
		public static class WebServicesAxisEmitter {
			/**
			* represents item : Web Services->Axis Emitter
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Axis Emitter";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class ServerDefaultFilesets {
			/**
			* represents item : Server->Default Filesets
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Default Filesets";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Server");
					return l;
				}
			};
			}
		public static class DataManagementConnectivityOpenDataAccessXMLDataSet {
			/**
			* represents item : Data Management->Connectivity->Open Data Access->XML Data Set
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "XML Data Set";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("Connectivity");
					l.add("Open Data Access");
					return l;
				}
			};
			}
		public static class JavaDebugStepFiltering {
			/**
			* represents item : Java->Debug->Step Filtering
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Step Filtering";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Debug");
					return l;
				}
			};
			}
		public static class JBossToolsWebElVariables {
			/**
			* represents item : JBoss Tools->Web->El Variables
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "El Variables";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					return l;
				}
			};
			}
		public static class JBossToolsWebStrutsStrutsPages {
			/**
			* represents item : JBoss Tools->Web->Struts->Struts Pages
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Struts Pages";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Struts");
					return l;
				}
			};
			}
		public static class JavaCodeStyleOrganizeImports {
			/**
			* represents item : Java->Code Style->Organize Imports
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Organize Imports";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Code Style");
					return l;
				}
			};
			}
		public static class TeamCVSUpdateMerge {
			/**
			* represents item : Team->CVS->Update/Merge
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Update/Merge";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					l.add("CVS");
					return l;
				}
			};
			}
		public static class ServerAudio {
			/**
			* represents item : Server->Audio
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Audio";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Server");
					return l;
				}
			};
			}
		public static class AntEditor {
			/**
			* represents item : Ant->Editor
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Editor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Ant");
					return l;
				}
			};
			}
		public static class GeneralEditorsTextEditors {
			/**
			* represents item : General->Editors->Text Editors
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Text Editors";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Editors");
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopmentSQLResultsViewOptionsExportFormatOptions {
			/**
			* represents item : Data Management->SQL Development->SQL Results View Options->Export Format Options
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Export Format Options";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("SQL Development");
					l.add("SQL Results View Options");
					return l;
				}
			};
			}
		public static class TeamIgnoredResources {
			/**
			* represents item : Team->Ignored Resources
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Ignored Resources";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Team");
					return l;
				}
			};
			}
		public static class GeneralWorkspace {
			/**
			* represents item : General->Workspace
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Workspace";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JBossToolsWebEditorsSeamPagesDiagram {
			/**
			* represents item : JBoss Tools->Web->Editors->Seam Pages Diagram
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Seam Pages Diagram";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("Editors");
					return l;
				}
			};
			}
		public static class JBossToolsWebStruts {
			/**
			* represents item : JBoss Tools->Web->Struts
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Struts";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					return l;
				}
			};
			}
		public static class DataManagement {
			/**
			* represents item : Data Management
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Data Management";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class Java {
			/**
			* represents item : Java
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Java";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JBossTools {
			/**
			* represents item : JBoss Tools
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JBoss Tools";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class JavaBuildPath {
			/**
			* represents item : Java->Build Path
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Build Path";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class ServerRuntimeEnvironments {
			/**
			* represents item : Server->Runtime Environments
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Runtime Environments";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Server");
					return l;
				}
			};
			}
		public static class WebServicesTestFacilityDefaults {
			/**
			* represents item : Web Services->Test Facility Defaults
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Test Facility Defaults";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class GeneralAppearance {
			/**
			* represents item : General->Appearance
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Appearance";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JBossToolsWebCDIValidator {
			/**
			* represents item : JBoss Tools->Web->CDI->Validator
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Validator";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Tools");
					l.add("Web");
					l.add("CDI");
					return l;
				}
			};
			}
		public static class DataManagementConnectivityDriverDefinitions {
			/**
			* represents item : Data Management->Connectivity->Driver Definitions
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Driver Definitions";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					l.add("Connectivity");
					return l;
				}
			};
			}
		public static class GeneralCapabilities {
			/**
			* represents item : General->Capabilities
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Capabilities";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class PluginDevelopmentOSGiFrameworks {
			/**
			* represents item : Plug-in Development->OSGi Frameworks
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "OSGi Frameworks";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Plug-in Development");
					return l;
				}
			};
			}
		public static class WebJavaServerFacesTools {
			/**
			* represents item : Web->JavaServer Faces Tools
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JavaServer Faces Tools";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			}
		public static class GeneralContentTypes {
			/**
			* represents item : General->Content Types
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Content Types";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class GeneralNetworkConnectionsCache {
			/**
			* represents item : General->Network Connections->Cache
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Cache";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Network Connections");
					return l;
				}
			};
			}
		public static class GeneralEditors {
			/**
			* represents item : General->Editors
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Editors";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class DataManagementSQLDevelopment {
			/**
			* represents item : Data Management->SQL Development
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "SQL Development";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					return l;
				}
			};
			}
		public static class FreeMarkerEditor {
			/**
			* represents item : FreeMarker Editor
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "FreeMarker Editor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class InstallUpdateAutomaticUpdates {
			/**
			* represents item : Install/Update->Automatic Updates
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Automatic Updates";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Install/Update");
					return l;
				}
			};
			}
		public static class GeneralSecurity {
			/**
			* represents item : General->Security
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Security";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					return l;
				}
			};
			}
		public static class JavaEditorTyping {
			/**
			* represents item : Java->Editor->Typing
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Typing";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Editor");
					return l;
				}
			};
			}
		public static class WebJavaScript {
			/**
			* represents item : Web->JavaScript
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JavaScript";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					return l;
				}
			};
			}
		public static class JBossjBPMjBPM3AssignmentTypes {
			/**
			* represents item : JBoss jBPM->jBPM 3->Assignment Types
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Assignment Types";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss jBPM");
					l.add("jBPM 3");
					return l;
				}
			};
			}
		public static class JavaPropertiesFilesEditor {
			/**
			* represents item : Java->Properties Files Editor
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Properties Files Editor";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					return l;
				}
			};
			}
		public static class Ant {
			/**
			* represents item : Ant
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Ant";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class DataManagementConnectivity {
			/**
			* represents item : Data Management->Connectivity
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Connectivity";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Data Management");
					return l;
				}
			};
			}
		public static class WebJavaServerFacesToolsViewsJSPTagRegistry {
			/**
			* represents item : Web->JavaServer Faces Tools->Views->JSP Tag Registry
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "JSP Tag Registry";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web");
					l.add("JavaServer Faces Tools");
					l.add("Views");
					return l;
				}
			};
			}
		public static class JavaCodeStyleCleanUp {
			/**
			* represents item : Java->Code Style->Clean Up
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Clean Up";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Code Style");
					return l;
				}
			};
			}
		public static class GeneralEditorsTextEditorsAccessibility {
			/**
			* represents item : General->Editors->Text Editors->Accessibility
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Accessibility";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Editors");
					l.add("Text Editors");
					return l;
				}
			};
			}
		public static class WebServicesAxis2Preferences {
			/**
			* represents item : Web Services->Axis2 Preferences 
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Axis2 Preferences ";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Web Services");
					return l;
				}
			};
			}
		public static class Help {
			/**
			* represents item : Help
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Help";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class RunDebugLaunchingLaunchConfigurations {
			/**
			* represents item : Run/Debug->Launching->Launch Configurations
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Launch Configurations";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Run/Debug");
					l.add("Launching");
					return l;
				}
			};
			}
		public static class JBossjBPMjBPM3ServerDeployment {
			/**
			* represents item : JBoss jBPM->jBPM 3->Server Deployment
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Server Deployment";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss jBPM");
					l.add("jBPM 3");
					return l;
				}
			};
			}
		public static class GeneralEditorsTextEditorsAnnotations {
			/**
			* represents item : General->Editors->Text Editors->Annotations
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Annotations";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Editors");
					l.add("Text Editors");
					return l;
				}
			};
			}
		public static class AntEditorTemplates {
			/**
			* represents item : Ant->Editor->Templates
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Templates";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Ant");
					l.add("Editor");
					return l;
				}
			};
			}
		public static class GeneralEditorsTextEditorsHyperlinking {
			/**
			* represents item : General->Editors->Text Editors->Hyperlinking
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Hyperlinking";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("General");
					l.add("Editors");
					l.add("Text Editors");
					return l;
				}
			};
			}
		public static class JavaBuildPathClasspathVariables {
			/**
			* represents item : Java->Build Path->Classpath Variables
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Classpath Variables";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Java");
					l.add("Build Path");
					return l;
				}
			};
			}
		public static class SWTBotPreferences {
			/**
			* represents item : SWTBot Preferences
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "SWTBot Preferences";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					return l;
				}
			};
			}
		public static class InstallUpdateAvailableSoftwareSites {
			/**
			* represents item : Install/Update->Available Software Sites
			*/
			public static final IPreference LABEL = new IPreference() {
				public String getName() { return "Available Software Sites";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Install/Update");
					return l;
				}
			};
			}
    public static class AtlassianConnectorUsageData {
      /**
      * represents item : Atlassian Connector -> Usage Data
      */
      public static final IPreference LABEL = new IPreference() {
        public String getName() { return "Usage Data";}
        public List<String> getGroupPath() {
          List<String> l = new Vector<String>();
          l.add("Atlassian Connector");
          return l;
        }
      };
      }
		}

	public static class ServerRuntime {
		public static class OracleOracleOC4JStandalone1013 {
			/**
			* represents item : Oracle->Oracle OC4J Standalone 10.1.3
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "Oracle OC4J Standalone 10.1.3";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Oracle");
					return l;
				}
			};
			/**
			* text field labeled 'Oracle J2EE Home:'
			*/
			public static final String TEXT_ORACLE_J2EE_HOME = "Oracle J2EE Home:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			}
		public static class JBossCommunityJBoss32Runtime {
			/**
			* represents item : JBoss Community->JBoss 3.2 Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JBoss 3.2 Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform43Runtime {
			/**
			* represents item : JBoss Enterprise Middleware->JBoss Enterprise Application Platform 4.3 Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JBoss Enterprise Application Platform 4.3 Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Enterprise Middleware");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossCommunityJBoss51Runtime {
			/**
			* represents item : JBoss Community->JBoss 5.1 Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JBoss 5.1 Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class ObjectWebJOnASv4 {
			/**
			* represents item : ObjectWeb->JOnAS v4
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JOnAS v4";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("ObjectWeb");
					return l;
				}
			};
			/**
			* text field labeled 'JonAS Configuration Directory:'
			*/
			public static final String TEXT_JONAS_CONFIGURATION_DIRECTORY = "JonAS Configuration Directory:";
			/**
			* text field labeled 'JonAS Installation Directory:'
			*/
			public static final String TEXT_JONAS_INSTALLATION_DIRECTORY = "JonAS Installation Directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			}
		public static class JBossCommunityJBoss50Runtime {
			/**
			* represents item : JBoss Community->JBoss 5.0 Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JBoss 5.0 Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossEnterpriseMiddlewareJBossEnterpriseApplicationPlatform5xRuntime {
			/**
			* represents item : JBoss Enterprise Middleware->JBoss Enterprise Application Platform 5.x Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JBoss Enterprise Application Platform 5.x Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Enterprise Middleware");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossCommunityJBoss42Runtime {
			/**
			* represents item : JBoss Community->JBoss 4.2 Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JBoss 4.2 Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class BasicJ2EEPreview {
			/**
			* represents item : Basic->J2EE Preview
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "J2EE Preview";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			}
		public static class BasicSCPRemoteDeployerRuntime {
			/**
			* represents item : Basic->SCP Remote Deployer Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "SCP Remote Deployer Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			}
		public static class BasicJ2EERuntimeLibrary {
			/**
			* represents item : Basic->J2EE Runtime Library
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "J2EE Runtime Library";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			/**
			* text field labeled 'Location:'
			*/
			public static final String TEXT_LOCATION = "Location:";
			}
		public static class JBossCommunityJBoss6xRuntime {
			/**
			* represents item : JBoss Community->JBoss 6.x Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JBoss 6.x Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossCommunityJBoss70Runtime {
			/**
			* represents item : JBoss Community->JBoss 7.0 Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JBoss 7.0 Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class JBossCommunityJBoss71Runtime {
			/**
			* represents item : JBoss Community->JBoss 7.1 Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JBoss 7.1 Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class ApacheApacheTomcatv32 {
			/**
			* represents item : Apache->Apache Tomcat v3.2
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "Apache Tomcat v3.2";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class ApacheApacheTomcatv55 {
			/**
			* represents item : Apache->Apache Tomcat v5.5
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "Apache Tomcat v5.5";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class ApacheApacheTomcatv60 {
			/**
			* represents item : Apache->Apache Tomcat v6.0
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "Apache Tomcat v6.0";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class BasicHTTPPreview {
			/**
			* represents item : Basic->HTTP Preview
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "HTTP Preview";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			}
		public static class OracleOracleOC4JStandalone1013n {
			/**
			* represents item : Oracle->Oracle OC4J Standalone 10.1.3.n
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "Oracle OC4J Standalone 10.1.3.n";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Oracle");
					return l;
				}
			};
			/**
			* text field labeled 'Oracle J2EE Home:'
			*/
			public static final String TEXT_ORACLE_J2EE_HOME = "Oracle J2EE Home:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			}
		public static class IBMIBMWebSpherev60 {
			/**
			* represents item : IBM->IBM WebSphere v6.0
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "IBM WebSphere v6.0";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("IBM");
					return l;
				}
			};
			/**
			* text field labeled 'IBM WebSphere Installation Directory:'
			*/
			public static final String TEXT_IBM_WEBSPHERE_INSTALLATION_DIRECTORY = "IBM WebSphere Installation Directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			}
		public static class ApacheApacheTomcatv50 {
			/**
			* represents item : Apache->Apache Tomcat v5.0
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "Apache Tomcat v5.0";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class JBossCommunityJBoss40Runtime {
			/**
			* represents item : JBoss Community->JBoss 4.0 Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "JBoss 4.0 Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("JBoss Community");
					return l;
				}
			};
			/**
			* text field labeled 'Directory:'
			*/
			public static final String TEXT_DIRECTORY = "Directory:";
			}
		public static class ApacheApacheTomcatv41 {
			/**
			* represents item : Apache->Apache Tomcat v4.1
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "Apache Tomcat v4.1";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class ApacheApacheTomcatv40 {
			/**
			* represents item : Apache->Apache Tomcat v4.0
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "Apache Tomcat v4.0";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Apache");
					return l;
				}
			};
			/**
			* text field labeled 'Tomcat installation directory:'
			*/
			public static final String TEXT_TOMCAT_INSTALLATION_DIRECTORY = "Tomcat installation directory:";
			/**
			* text field labeled 'JRE:'
			*/
			public static final String TEXT_JRE = "JRE:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class BasicHTTPServer {
			/**
			* represents item : Basic->HTTP Server
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "HTTP Server";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			/**
			* text field labeled 'Publishing Directory:'
			*/
			public static final String TEXT_PUBLISHING_DIRECTORY = "Publishing Directory:";
			/**
			* text field labeled 'Name:'
			*/
			public static final String TEXT_NAME = "Name:";
			}
		public static class BasicLocalDeployerRuntime {
			/**
			* represents item : Basic->Local Deployer Runtime
			*/
			public static final IServerRuntime LABEL = new IServerRuntime() {
				public String getName() { return "Local Deployer Runtime";}
				public List<String> getGroupPath() {
					List<String> l = new Vector<String>();
					l.add("Basic");
					return l;
				}
			};
			}

		}

}
