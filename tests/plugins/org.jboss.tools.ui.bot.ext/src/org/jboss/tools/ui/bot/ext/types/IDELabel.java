 /*******************************************************************************
  * Copyright (c) 2007-2009 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.ui.bot.ext.types;

/**
 * Base label constants for all widgets. Naming convention is (except buttons
 * and menus) based on Eclipse platform class names of each part (e.g.
 * NewJavaProjectWizardPageOne)
 * 
 * @author jpeterka 
 */
public class IDELabel {
	public class Menu {
		public static final String FILE = "File";
		public static final String NEW = "New";
		public static final String PROJECT = "Project";
		public static final String OTHER = "Other...";
		public static final String WINDOW = "Window";
		public static final String SHOW_VIEW = "Show View";
		public static final String OPEN_PERSPECTIVE = "Open Perspective";
		public static final String OPEN_WITH =  "Open With";
		public static final String TEXT_EDITOR = "Text Editor";
		public static final String EDIT = "Edit";
		public static final String SELECT_ALL = "Select All";
		public static final String CLOSE = "Close";
		public static final String OPEN = "Open";
		public static final String RENAME = "Rename";
		public static final String JSP_FILE = "JSP File";
		public static final String PROPERTIES = "Properties";
		public static final String XHTML_FILE = "XHTML File";
		public static final String HELP = "Help";
		public static final String ABOUT_JBOSS_DEVELOPER_STUDIO = "About JBoss Developer Studio";
		public static final String HIBERNATE_CODE_GENERATION = "Hibernate Code Generation...";
		public static final String HIBERNATE_CODE_GENERATION_CONF = "Hibernate Code Generation Configurations...";
		public static final String REMOVE = "Remove";
		public static final String IMPORT = "Import...";
		public static final String RUN_AS = "Run As";
		public static final String WEB_PROJECT_JBT_JSF = "JBoss Tools JSF";
		public static final String PACKAGE_EXPLORER_JBT = "JBoss Tools";
		public static final String PACKAGE_EXPLORER_CONFIGURE = "Configure";
		public static final String ADD_JSF_CAPABILITIES = "Add JSF Capabilities...";
		public static final String CLOSE_PROJECT = "Close Project";
		public static final String OPEN_PROJECT = "Open Project";
		public static final String DELETE = "Delete";
		public static final String JBT_REMOVE_JSF_CAPABILITIES = "Remove JSF Capabilities";
		public static final String START = "Start";
		public static final String STOP = "Stop";
		public static final String STRUTS_PROJECT = "Struts Project";
		public static final String PREFERENCES = "Preferences";
		public static final String JBT_REMOVE_STRUTS_CAPABILITIES = "Remove Struts Capabilities";
    public static final String ADD_STRUTS_CAPABILITIES = "Add Struts Capabilities...";
    public static final String WEB_PROJECT_JBT_STRUTS = "JBoss Tools Struts";
    public static final String RUN = "Run";
    public static final String RUN_ON_SERVER = "Run on Server";
    public static final String ADD_AND_REMOVE="Add and Remove...";
    public static final String RUN_AS_JAVA_APPLICATION="Java Application";
    public static final String TOGGLE_BREAKPOINT="Toggle Breakpoint";
    public static final String DEBUG_AS = "Debug As";
    public static final String DEBUG_AS_DROOLS_APPLICATION = "Drools Application";
    public static final String OPEN_GUVNOR_CONSOLE = "Open Guvnor Console";
    public static final String GUVNOR = "Guvnor";
    public static final String GUVNOR_UPDATE = "Update";
    public static final String GUVNOR_COMMIT = "Commit";
    public static final String GUVNOR_ADD = "Add...";
    public static final String GUVNOR_DELETE = "Delete...";
    public static final String GUVNOR_DISCONNECT = "Disconnect";
    public static final String GUVNOR_SHOW_HISTORY = "Show History";
    public static final String GUVNOR_COMPARE_WITH_VERSION = "Compare with Version...";
    public static final String GUVNOR_SWITCH_TO_VERSION = "Switch to Version...";
    public static final String NAVIGATION = "Navigation";
    public static final String MAXIMIZE_ACTIVE_VIEW_OR_EDITOR = "Maximize Active View or Editor";
    public static final String REFRESH = "Refresh";
    public static final String CUT = "Cut";
    public static final String COPY = "Copy";
    public static final String PASTE = "Paste";
    public static final String UNDO = "Undo";
    public static final String CLOSE_ALL = "Close All";
    public static final String SAVE_AS_TEMPLATE = "Save As Template...";
    public static final String AUTO_LAYOUT = "Auto Layout";
    public static final String CDI_REFACTOR = "CDI Refactor";
	}

	public class Button {
		public static final String NEXT = "Next >";
		public static final String BACK = "< Back";
		public static final String CANCEL = "Cancel";
		public static final String FINISH = "Finish";
		public static final String OK = "OK";
		public static final String YES = "Yes";
		public static final String NO = "No";
		public static final String CLOSE = "Close";
		public static final String RUN = "Run";
		public static final String APPLY = "Apply";
		public static final String ADD = "Add...";
		public static final String NEW = "New...";
		public static final String CONTINUE = "Continue";
		public static final String REMOVE = "Remove";
		public static final String REMOVE_WITH_DOTS = "Remove...";
		public static final String EDIT = "Edit...";
		public static final String ADD_WITHOUT_DOTS = "Add";
		public static final String EDIT_WITHOUT_DOTS = "Edit";
		public static final String DROOLS_WORKBENCH = "Drools workbench";
		public static final String REFRESH = "Refresh";
		public static final String TEST = "Test";
		public static final String DELETE = "Delete";
		public static final String UP = "Up";
		public static final String DOWN = "Down";
	}

	public class Shell {
		public static final String NEW_JAVA_PROJECT = "New Java Project";
		public static final String NEW_JAVA_CLASS = "New Java Class";
		// JBT
		// public static final String NEW_HIBERNATE_MAPPING_FILE = "New Hibernate XML Mapping file (hbm.xml)";
		// JBDS
		public static final String NEW_HIBERNATE_MAPPING_FILE = "Create Hibernate XML Mapping file (hbm.xml)";		
		public static final String NEW = "New";
		public static final String SAVE_RESOURCE = "Save Resource";
		public static final String RENAME_RESOURCE = "Rename Resource";
		public static final String NEW_JSP_FILE = "New JSP File";
		public static final String PROPERTIES = "Properties";
		public static final String NEW_XHTML_FILE = "New XHTML Page";
		public static final String IMPORT_JSF_PROJECT = "Import JSF Project";
		public static final String IMPORT = "Import";
		public static final String DELETE_SERVER = "Delete Server";
		public static final String NEW_STRUTS_PROJECT = "New Struts Project";
	  public static final String PREFERENCES = "Preferences";
	  public static final String NEW_SERVER_RUNTIME_ENVIRONMENT = "New Server Runtime Environment";
	  public static final String OPEN_ASSOCIATED_PERSPECTIVE = "Open Associated Perspective?";
	  public static final String DELETE_RESOURCES = "Delete Resources";
	  public static final String IMPORT_STRUTS_PROJECT = "Import Struts Project";
	  public static final String UNSUPPORTED_CONTENT_TYPE = "Unsupported Content Type";
	  public static final String NEW_SERVER = "New Server";
	  public static final String RUN_ON_SERVER = "Run On Server";
	  public static final String WARNING = "Warning";
	  public static final String DROOLS_RUNTIME = "Drools Runtime";
	  public static final String NEW_DROOLS_PROJECT = "";
	  public static final String RENAME_COMPILATION_UNIT = "Rename Compilation Unit";
	  public static final String RENAME_JAVA_PROJECT = "Rename Java Project";
	  public static final String CONFIRM_PERSPECTIVE_SWITCH = "Confirm Perspective Switch";
	  public static final String NEW_SEAM_RUNTIME = "New Seam Runtime";
	  public static final String NEW_ESB_RUNTIME = "New JBoss ESB Runtime";
	  public static final String NEW_JBPM_RUNTIME = "Add Location";
	  public static final String CONFIRM_DELETE = "Confirm Delete";  
	  public static final String SHOW_VIEW = "Show View";
	  public static final String PROPERTIES_FOR = "Properties for";
	  public static final String COPY_FILE_FROM_GUVNOR_TO_PACKAGE_EXPLORER = "File Operation";
	  public static final String INSERT_TAG = "Insert Tag";
	  public static final String SHOW_HIDE_DRAWERS = "Show/Hide Drawers";
	  public static final String PALETTE_EDITOR = "Palette Editor";
	  public static final String CREATE_GROUP = "Create Group";
	  public static final String ADD_PALETTE_GROUP = "Add Palette Group";
	  public static final String ADD_PALETTE_MACRO = "Add Palette Macro";
	  public static final String CONFIRMATION = "Confirmation";
	  public static final String USER_SPECIFIED_TAG_TEMPLATE = "User specified tag template";
	  public static final String PAGE_DESIGN_OPTIONS = "Page Design Options";
	  public static final String ADD_TAGLIB_REFRENCE = "Add Taglib Reference";
	  public static final String ADD_EL_REFERENCE = "Add EL Reference";
	  public static final String ADD_CSS_REFERENCE = "Add CSS Reference";
	  public static final String NEW_CSS_FILE = "New CSS File";
	  public static final String NEW_HTML_FILE = "New HTML File";
	  public static final String ADD_SVN_REPOSITORY = "Add SVN Repository";
	  public static final String REPOSITORY_LOCATION_PROPERTIES = "Repository Location Properties";
	  public static final String SECURE_STORAGE = "Secure Storage";
	  public static final String ADD_JSF_PROJECT_TEMPLATE = "Add JSF Project Template";
	  public static final String NEW_MANAGED_BEAN = "New Managed Bean";
	  public static final String NEW_MANAGED_BEAN_JSF2 = "Managed Bean...";
	  public static final String ADD_COMPONENT = "Add Component";
	  public static final String ADD_CONVERTER = "Add Converter";
	  public static final String ADD_REFERENCED_BEAN = "Add Referenced Bean";
	  public static final String ADD_RENDER_KIT = "Add Render Kit";
	  public static final String ADD_RENDER_KIT_JSF2 = "Render Kit...";
	  public static final String ADD_VALIDATOR = "Add Validator";
	  public static final String NEW_VIEW = "New View";
	  public static final String AUTO_LAYOUT = "Auto Layout";
	  public static final String ADD_PROPERTY = "Add Property";
	  public static final String EDIT = "Edit";	  
	}

	public class EntityGroup {
		public static final String HIBERNATE = "Hibernate";
		public static final String JAVA = "Java";
		public static final String SEAM = "Seam";
		public static final String STRUTS = "Struts";
		public static final String JBOSS_TOOLS_WEB = "JBoss Tools Web";
		public static final String JPA = "JPA";
		public static final String DROOLS = "Drools";
		public static final String GUVNOR = "Guvnor";		
		public static final String SMOOKS = "Smooks";
		public static final String JBPM = "JBoss jBPM";
	}
	
	public class EntityLabel {
		public static final String HIBERNATE_MAPPING_FILE = "Hibernate XML Mapping file (hbm.xml)";
		public static final String HIBERNATE_REVERSE_FILE = "Hibernate Reverse Engineering File (reveng.xml)";
		public static final String HIBERNATE_CONSOLE = "Hibernate Console Configuration";
		public static final String JAVA_CLASS = "Class";
		public static final String JAVA_PROJECT =  "Java Project";
		public static final String JAVA_PACKAGE =  "Package";
		public static final String SEAM_PROJECT = "Seam Web Project";
		public static final String HIBERNATE_CONFIGURATION_FILE = "Hibernate Configuration File (cfg.xml)";
		public static final String STRUTS_PROJECT = "Struts Project";
		public static final String JPA_PROJECT = "JPA Project";
		public static final String DROOLS_PROJECT = "Drools Project";
		public static final String DROOLS_RULE = "Rule Resource";
		public static final String GUIDED_DROOLS_RULE = "Guided Rule";
		public static final String DSL_DROOLS_FILE = "Domain Specific Language";
		public static final String SMOOKS_CONF_FILE = "Smooks Configuration File";
		public static final String RESOURCES_FROM_GUVNOR = "Resources from Guvnor";
		public static final String JBPM3_PROJECT = "jBPM 3 Project";
	}

	public class JavaProjectWizard {
		public static final String PROJECT_NAME = "Project name:";
	}

	public class NewClassCreationWizard {
		public static final String CLASS_NAME = "Name:";
		public static final String PACKAGE_NAME = "Package:";
	}

	public class ShowViewDialog {
		public static final String JAVA_GROUP = "Java";
		public static final String PROJECT_EXPLORER = "Project Explorer";

	}

	public class View {
		public static final String WELCOME = "Welcome";
		public static final String PROJECT_EXPLORER = "Project Explorer";
		public static final String PACKAGE_EXPLORER = "Package Explorer";
		public static final String DATA_SOURCE_EXPLORER = "Data Source Explorer";
		public static final String SERVERS = "Servers";
		public static final String WEB_PROJECTS = "Web Projects";
		public static final String PROBLEMS = "Problems";
		public static final String DEBUG = "Debug";
		public static final String GUVNOR_REPOSITORIES = "Guvnor Repositories";
		public static final String GUVNOR_RESOURCE_HISTORY = "Guvnor Resource History";
		public static final String PROPERTIES = "Properties";
		public static final String JBOSS_TOOLS_PALETTE = "JBoss Tools Palette";
		public static final String PALETTE = "Palette";
		public static final String JBOSS_CENTRAL = "JBoss Central";
	}
	
	public class ViewGroup {
		public static final String GENERAL = "General";
		public static final String JAVA = "Java";
		public static final String DATA_MANAGEMENT = "Data Management";
		public static final String SERVER = "Server";
		public static final String JBOSS_TOOLS_WEB = "JBoss Tools Web";
		public static final String DEBUG = "Debug";
		public static final String GUVNOR = "Guvnor";
	}

	public class SelectPerspectiveDialog {
		public static final String JAVA = "Java";		
		public static final String CDI = "Context and Dependency Injection (CDI)";
		public static final String HIBERNATE = "Hibernate";
		public static final String SEAM = "Seam";
		public static final String WEB_DEVELOPMENT = "Web Development";
		public static final String DB_DEVELOPMENT = "Database Development";
		public static final String JPA = "JPA";
		public static final String DEBUG = "Debug";
		public static final String GUVNOR_REPOSITORY_EXPLORING = "Guvnor Repository Exploring";
		public static final String DROOLS = "Drools";
		public static final String JBPM3 = "jBPM jPDL 3";
	}
	/**
	 * Hibernate Console Wizard (ConsoleConfigurationCreationWizard) Labels (
	 * @author jpeterka
	 *
	 */
	public class HBConsoleWizard {
		public static final String MAIN_TAB = "Main";
		public static final String OPTIONS_TAB = "Options";
		public static final String CLASSPATH_TAB = "Classpath";
		public static final String MAPPINGS_TAB = "Mappings";
		public static final String COMMON_TAB = "Common";
		public static final String PROJECT_GROUP = "Project:";
		public static final String CONFIGURATION_FILE_GROUP = "Configuration file:";
		public static final String SETUP_BUTTON = "Setup...";
		public static final String CREATE_NEW_BUTTON = "Create new...";
		public static final String USE_EXISTING_BUTTON = "Use existing...";
		public static final String DATABASE_DIALECT = "Database dialect:";
		public static final String DRIVER_CLASS = "Driver class:";
		public static final String CONNECTION_URL = "Connection URL:";
		public static final String USERNAME = "Username:";
		public static final String CREATE_CONSOLE_CONFIGURATION = "Create a console configuration";
	}
	
	public class HBLaunchConfigurationDialog {

		public static final String MAIN_TAB = "Main";
		public static final String EXPORTERS_TAB = "Exporters";
		public static final String REFRESH_TAB = "Refresh";
		public static final String COMMON_TAB = "Common";
		
	}
	
	public class HBConfigurationWizard {
		public static final String FILE_NAME = "File name:";
	}

	public static class RenameResourceDialog {

		public static final String NEW_NAME = "New name:";
		
	}
	
	public static class WebProjectsTree {

		public static final String WEB_CONTENT = "WebContent";
		public static final String CONFIGURATION = "Configuration";
		public static final String WEB_XML = "web.xml";
		public static final String CONTEXT_PARAMS = "Context Params";
		public static final String JAVAX_FACES_CONFIG_FILES = "javax.faces.CONFIG_FILES";
		public static final String DEFAULT = "default";
		public static final String SERVLETS = "Servlets";
		public static final String ACTION_STRUTS = "action:org.apache.struts.action.ActionServlet";
		public static final String CONFIG = "config";
		public static final String TAG_LIBRARIES = "Tag Libraries";
		
	}
	
	public static class NewJSPFileDialog {

		public static final String NAME = "Name*";
		public static final String TEMPLATE = "Template";
		public static final String TEMPLATE_JSF_BASE_PAGE = "JSFBasePage";
		public static final String JSP_TEMPLATE = "New JSP File (html)";
		
	}
	
	public static class PropertiesDialog {

		public static final String PARAM_VALUE = "Param-Value";
		
	}

	public static final class NewXHTMLFileDialog {

		public static final String FILE_NAME = "Name*";
		public static final String TEMPLATE = "Template";
		public static final String TEMPLATE_FACELET_FORM_XHTML = "FaceletForm.xhtml";
		public static final String TEMPLATE_FACELET_FORM_XHTML_NAME = "Form Facelet Page";
		public static final String USE_XHTML_TEMPLATE_CHECK_BOX = "Use XHTML Template";
		
	}
	
	public static final class ServerName {

	  public static final String JBOSS_EAP_4_3_RUNTIME_SERVER = "JBoss EAP 4.3 Runtime Server";
	  // Server with this Label is created during JBDS installation for bundled EAP
	  public static final String JBOSS_EAP = "jboss-eap";
	    
	}
	 
   public static final class ServerRuntimeName {

     public static final String JBOSS_EAP_4_3 = "JBoss EAP 4.3 Runtime";
     // Server Runtime with this Label is created during JBDS installation for bundled EAP
     public static final String JBOSS_EAP = "jboss-eap Runtime";
     public static final String JBOSS_EAP_5_0 = "JBoss EAP 5.0 Runtime";
     public static final String JBOSS_AS_5_1 = "JBoss 5.1 Runtime";

   }
	
   public static final class ServerJobName {

     public static final String STARTING_JBOSS_EAP_43_RUNTIME = "Starting JBoss EAP 4.3 Runtime Server";
     public static final String STOPPING_JBOSS_EAP_43_RUNTIME = "Stoppig JBoss EAP 4.3 Runtime Server";
     public static final String STARTING_JBOSS_EAP = "Starting jboss-eap";
     public static final String STOPPING_JBOSS_EAP = "Stopping jboss-eap";
     
   }
   
   public static class ImportJSFProjectDialog {

     public static final String RUNTIME = "Runtime*";
     public static final String CHOICE_LIST_IS_EMPTY = "Choice list is empty.";
     
   }
   
   public class NewStrutsProjectDialog{
     
     public static final String NAME = "Project Name*";
     public static final String TEMPLATE = "Template*";
     public static final String TEMPLATE_KICK_START = "KickStart";
     
   }
   
   public static class PreferencesDialog {

     public static final String SERVER_GROUP = "Server";
     public static final String RUNTIME_ENVIRONMENTS = "Runtime Environments";
     public static final String DROOLS_GROUP = "Drools";
     public static final String INSTALLED_DROOLS_RUNTIMES = "Installed Drools Runtimes";  
     public static final String JBOSS_TOOLS = "JBoss Tools";
     public static final String JBOSS_TOOLS_WEB = "Web";
     public static final String JBOSS_TOOLS_WEB_EDITORS = "Editors";
     public static final String JBOSS_TOOLS_WEB_EDITORS_VPE = "Visual Page Editor";
     public static final String JBOSS_TOOLS_WEB_EDITORS_VPE_VISUAL_TEMPLATES = "Visual Templates";
     public static final String JBOSS_TOOLS_EXPRESSION_LANGUAGE = "Expression Language";
     public static final String JBOSS_TOOLS_VARIABLES = "Variables";
   }
   
   public static class JBossServerRuntimeDialog {

     public static final String NAME = "Name";
     public static final String HOME_DIRECTORY = "Home Directory";
     
   }
   public static final class ServerGroup {

     public static final String JBOSS_EAP_4_3 = "JBoss Enterprise Middleware";
     public static final String JBOSS_EAP_5_0 = "JBoss Enterprise Middleware";
     public static final String JBOSS_EAP_5_x = "JBoss Enterprise Middleware";
     public static final String JBOSS_COMMUNITY = "JBoss Community";
     public static final String JBOSS_AS_6_0 = "JBoss Community";
       
   }
   public static final class ServerRuntimeType {

     public static final String JBOSS_EAP_4_3 = "JBoss Enterprise Application Platform 4.3 Runtime";
     public static final String JBOSS_EAP_5_0 = "JBoss Enterprise Application Platform 5.0 Runtime";
     public static final String JBOSS_EAP_5_x = "JBoss Enterprise Application Platform 5.x Runtime";
     public static final String JBOSS_AS_5_1 = "JBoss 5.1 Runtime";
     public static final String JBOSS_AS_6_0 = "JBoss 6.0 Runtime";
     public static final String JBOSS_AS_6_x = "JBoss 6.x Runtime";
       
   }
   public static final class ServerType {

     public static final String JBOSS_EAP_4_3 = "JBoss Enterprise Application Platform 4.3";
     public static final String JBOSS_EAP_5_0 = "JBoss Enterprise Application Platform 5.0";
     public static final String JBOSS_EAP_5_x = "JBoss Enterprise Application Platform 5.x";
     public static final String JBOSS_AS_6_0 = "JBoss 6.0 Runtime";
     public static final String JBOSS_AS_6_x = "JBoss 6.x Runtime";
     
   }
   public static final class DroolsRuntimeDialog {

     public static final String NAME = "Name: ";
     public static final String PATH = "Path: ";
     public static final int COLUMN_NAME_INDEX = 0;
     public static final int COLUMN_LOCATION_INDEX = 1;
       
   }
   public static final class NewDroolsProjectDialog {

     public static final String NAME = "Project name:";
     public static final String GENERATE_CODE_COMPATIBLE_WITH_COMBO_BOX_LABEL = "Generate code compatible with: ";
     public static final String CODE_COMPATIBLE_WITH_51_DROOLS_OR_ABOVE = "Drools 5.1 or above";
     public static final String CODE_COMPATIBLE_WITH_50_DROOLS = "Drools 5.0.x";
       
   }
   
   public static final class NewDroolsRuleDialog {

     public static final String FILE_NAME = "File name:";
     public static final String RULE_PACKAGE_NAME = "Rule package name:";
       
   }
   
   public static class ProblemsTree {

     public static final String WARNINGS = "Warnings";
     public static final String ERRORS = "Errors";
     
   }
   
   public static class ConsoleView {

     public static final String BUTTON_CLEAR_CONSOLE_TOOLTIP = "Clear Console";
     public static final String BUTTON_DISPLAY_SELECTED_CONSOLE_TOOLTIP = "Display Selected Console";
     public static final String BUTTON_SHOW_WHEN_STDOUT_CHANGES_TOOLTIP = "Show Console When Standard Out Changes";
     public static final String BUTTON_SHOW_WHEN_STDERR_CHANGES_TOOLTIP = "Show Console When Standard Error Changes";
     
   }
   
   public static class DebugView {

     public static final String BUTTON_STEP_OVER_TOOLTIP = "Step Over (F6)";
     public static final String BUTTON_RESUME_TOOLTIP = "Resume (F8)";
     
   }
   
   public static class DroolsEditor {

     public static final String TEXT_EDITOR_TAB = "Text Editor";
     public static final String RETE_TREE_TAB = "Rete Tree";
     
   }
   
   public static final class NewGuidedDroolsRuleDialog {

     public static final String FILE_NAME = "File name:";
       
   }
   
   public static final class GuidedDroolsRuleEditor {

     public static final String WHEN_ADD_DIALOG_TITLE = "Add new condition to the rule";
     public static final String WHEN_ADD_FACT_COMBO = "Fact";
     public static final String ADD_FIELD_TO_THIS_CONDITION_TOOLTIP = "Add a field to this condition, or bind a varible to this fact.";
     public static final String REMOVE_THIS_CONDITION_TOOLTIP = "Remove this condition.";
     public static final String UPDATE_CONSTRAINTS_DIALOG_TITLE = "Update constraints";
     public static final String ADD_RESTRICTION_ON_A_FIELD_COMBO_VALUE = "empty";
     public static final String WHEN_COMBO_CONSTRAINTS_VALUE = "is equal to";
     public static final String CHOOSE_VALUE_EDITOR_TYPE_TOOLTIP = "Choose value editor type";
     public static final String SELECT_VALUE_EDITOR_TYPE_DIALOG_TITLE = "Select value editor type";
     public static final String SELECT_VALUE_EDITOR_TYPE_COMBO_LABEL = "Field value:";
     public static final String SELECT_VALUE_EDITOR_TYPE_COMBO_VALUE = "Literal value";
     public static final String FIELD_VALUE_COMBO_VALUE = "true";
       
   }
   
   public static final class NewDslDroolsFileDialog {

     public static final String FILE_NAME = "File name:";
       
   }
   
   public static final class DslDroolsFileEditor {
     
     public static final String ADD_LANGUAGE_MAPPING_DIALOG_TITLE = "New language mapping";
     public static final String LANGUAGE_EXPRESSION_TEXT_LABEL = "Language expression:";
     public static final String RULE_MAPPING_TEXT_LABEL = "Rule mapping:";
     public static final String SCOPE_COMBO_LABEL = "Scope:";
     public static final String SCOPE_COMBO_VALUE = "condition";
     
   }
   
   public static final class GuvnorRepositories {
     
     public static final String ADD_GUVNOR_REPOSITORY_TOOLTIP = "Add a Guvnor respository connection";
     public static final String REMOVE_GUVNOR_REPOSITORY_TOOLTIP = "Delete Guvnor repository connection";
     public static final String REMOVE_GUVNOR_REPOSITORY_DIALOG_TITLE = "Remove repository connection";
     public static final String PACKAGES_TREE_ITEM = "packages/";
     public static final String MORTGAGE_TREE_ITEM = "mortgages/";
     public static final String GO_INTO_GUVNOR_REPOSITORY_TOOLTIP = "Go Into";
     public static final String BACK_GUVNOR_REPOSITORY_TOOLTIP = "Back";
     public static final String HOME_GUVNOR_REPOSITORY_TOOLTIP = "Home";
     public static final String DEFAULT_PACKAGE_TREE_ITEM = "defaultPackage/";
     public static final String APPLICANTDSL_DSL_TREE_ITEM = "ApplicantDsl.dsl";
   }
   
   public static final class GuvnorConsole {
     
     public static final String GUVNOR_CONSOLE_TITLE = "JBoss BRMS";
     public static final String BUTTON_YES_INSTALL_SAMPLES = "Yes, please install samples";
     
   }
   
   public static class PropertiesWindow {

     public static final String RESOURCE = "Resource";
     
     public static final class ResourceProperties {
       
       public static final String PATH = "Path:";
       public static final String LOCATION = "Location:";
       
     }
   }
   
   public static class GuvnorPropertiesDialog {

     public static final String GUVNOR_URL_TEMPLATE = "Guvnor URL template: ";
     
   }
   
   public static class JsfProjectTree {

     public static final String WEB_CONTENT = "WebContent";
     public static final String PAGES = "pages";
     
   }
   
   public static class VisualPageEditor{
     
     public static final String SOURCE_TAB_LABEL = "Source";
     public static final String VISUAL_SOURCE_TAB_LABEL = "Visual/Source";
     public static final String PREVIEW_TAB_LABEL = "Preview";
     public static final String BOLD_TOOLBAR_BUTTON_LABEL = "Bold";
     public static final String ITALIC_TOOLBAR_BUTTON_LABEL = "Italic";
     public static final String UNDERLINE_TOOLBAR_BUTTON_LABEL = "Underline";
     
   }

   public static class JBossToolsPalette{
     
     public static final String IMPORT_TOOL_ITEM = "Import";
     public static final String PALETTE_EDITOR_TOOL_ITEM = "Palette Editor";
     public static final String SHOW_HIDE_TOOL_ITEM = "Show/Hide";

   }
   
   public static class PaletteEditor{
     
     public static final String XSTUDIO_NODE = "XStudio";
     public static final String PALETTE_NODE = "Palette";
     public static final String NEW_MENU_ITEM = "New";
     public static final String CREATE_GROUP_MENU_ITEM = "Create Group...";
     public static final String CREATE_MACRO_MENU_ITEM = "Create Macro...";
     public static final String DELETE_MENU_ITEM = "Delete";

   }
   
   public static class CreateGroupDialog{
     
     public static final String NAME = "Name:*";

   }
   
   public static class AddPaletteGroupDialog{
     
     public static final String NAME = "Name:*";

   }

   public static class AddPaletteMacroDialog{
     
     public static final String NAME = "Name:*";
     public static final String START_TEXT = "Start Text:";
     public static final String END_TEXT = "End Text:";

   }
   
   public static class UserSpecifiedTagTemplateDialog{
     
     public static final String TAG_FOR_DISPLAY = "Tag for display:";

   }
   
   public static class PageDesignOptionsDialog{
     
     public static final String INCLUDED_TAG_LIBS_TAB = "Included tag libs";
     public static final String INCLUDED_TAG_LIBS_URI = "URI*";
     public static final String INCLUDED_TAG_LIBS_PREFIX = "Prefix";
     
     public static final String SUBSTITUTED_EL_EXPRESSIONS_TAB = "Substituted EL expressions";
     public static final String SUBSTITUTED_EL_EXPRESSIONS_EL_NAME = "El Name*";
     public static final String SUBSTITUTED_EL_EXPRESSIONS_VALUE = "Value";
     
     public static final String SUBSTITUTED_EL_EXPRESSIONS_SCOPE_PAGE = "Page: Only This Page";
     public static final String SUBSTITUTED_EL_EXPRESSIONS_SCOPE_FOLDER = "Folder: Any Page at the Same Folder";
     public static final String SUBSTITUTED_EL_EXPRESSIONS_SCOPE_PROJECT = "Project: Any Page at the Same Project";
     public static final String SUBSTITUTED_EL_EXPRESSIONS_FOLDER_SCOPE_TABLE_LABEL = "Folder";
     
     public static final String INCLUDED_CSS_FILES_TAB = "Included css files";
     public static final String INCLUDED_CSS_FILES_CSS_FILE_PATH = "CSS File Path*";
     
   }   
   
   public static class AddTaglibReferenceDialog{
     
     public static final String INCLUDED_TAG_LIBS_TAB = "Included tag libs";

   } 
   
   public static class GuvnorConsoleLoginDialog{
     
     public static final String USER_NAME = "User Name: ";
     public static final String PASSWORD = "Password:  ";

   }
   
   public class NewHTMLWizard {
     public static final String FILE_NAME = "File name:";
   }
   
   public class NewCSSWizard {
     public static final String FILE_NAME = "File name:";
   }

   public static class AddELReferenceDialog{
     
     public static final String EL_NAME = "El Name*";
     public static final String VALUE = "Value";
     
   }  
   
   public static class ToolbarButton{
     
     public static final String REFRESH = "Refresh";
     
   }
   
   public static final class GuvnorAddRepositoryDialog {
     
     public static final String REPOSITORY = "Repository: ";
   }
   
   public static class FacesConfigEditor{
     
     public static final String SOURCE_TAB_LABEL = "Source";
     public static final String TREE_TAB_LABEL = "Tree";
     public static final String DIAGRAM_TAB_LABEL = "Diagram";
     public static final String MANAGED_BEANS_NODE = "Managed Beans";
     public static final String COMPONENTS_NODE = "Components";
     public static final String CONVERTERS_NODE = "Converters";
     public static final String RENDER_KITS_NODE = "Render Kits";
     public static final String VALIDATOR_NODE = "Validators";
     public static final String REFERENCED_BEAN_NODE = "Referenced Beans";
     public static final String NEW_MANAGED_BEAN_CLASS_LABEL = "Class:*";
     public static final String MANAGED_BEAN_CLASS_LABEL = "Managed Bean Class:"; 
     public static final String NEW_MANAGED_BEAN_NAME_LABEL = "Name:*";
     public static final String DELETE_JAVA_SOURCE_CHECK_BOX = "Delete Java Source";
     public static final String NEW_COMPONENT_TYPE_LABEL = "Component Type:*";
     public static final String NEW_COMPONENT_CLASS_LABEL = "Component Class:*";
     public static final String NEW_CONVERTER_ID_LABEL = "Converter ID:*";
     public static final String NEW_CONVERTER_CLASS_LABEL = "Converter Class:*";
     public static final String NEW_REFERENCED_BEAN_NAME_LABEL = "Referenced Bean Name:*";
     public static final String NEW_REFERENCED_BEAN_CLASS_LABEL = "Referenced Bean Class:*";
     public static final String NEW_RENDER_KIT_ID_LABEL = "Render Kit ID:";
     public static final String NEW_RENDER_KIT_CLASS_LABEL = "Render Kit Class:";
     public static final String NEW_VALIDATOR_ID_LABEL = "Validator ID:*";
     public static final String NEW_VALIDATOR_CLASS_LABEL = "Validator Class:*";
     public static final String GEF_VIEW_TEMPLATE_TOOL = "View Template";
     public static final String GEF_CREATE_NEW_CONNECTION_TOOL = "Create New Connection";
     public static final String DELETE_FILE_FROM_DISK_CHECK_BOX = "Delete file from disk";
   } 
   
   public static class NewJsfProjectDialog{
     
     public static final String PROJECT_NAME_LABEL = "Project Name*";
     public static final String JSF_ENVIRONMENT_LABEL = "JSF Environment*";
     public static final String TEMPLATE_LABEL = "Template*";
     public static final String RUNTIME_LABEL = "Runtime:*";
     public static final String HOME_DIRECTORY_LABEL = "Home Directory";
   }
   
   public static class SVNRepositoriesView {

     public static final String ADD_SVN_REPOSITORY_TOOLTIP = "Add SVN Repository";
     
   }
   
   public static class AddSVNRepositoryDialog {

     public static final String URL_TEXT_LABEL = "Url:";
     
   }
   
   public static class WebXmlEditor{
     
     public static final String SOURCE_TAB_LABEL = "Source";
     public static final String TREE_TAB_LABEL = "Tree";
     public static final String ADD_SERVLET_DIALOG_TITLE = "Add Servlet";
     public static final String ADD_SERVLET_MAPPING_DIALOG_TITLE = "Add Servlet Mapping";
     public static final String ADD_SERVLET_DIALOG_SERVLET_NAME_LABEL = "Servlet-Name:*";
     public static final String ADD_SERVLET_DIALOG_DISPLAY_NAME_LABEL = "Display-Name:";
     public static final String ADD_SERVLET_DIALOG_SERVLET_CLASS_LABEL = "Servlet-Class:*";
     public static final String ADD_SERVLET_DIALOG_DESCRITPION_LABEL = "Description:";
     public static final String ADD_SERVLET_MAPPING_DIALOG_SERVLET_NAME_LABEL = "Servlet-Name:*";
     public static final String ADD_SERVLET_MAPPING_DIALOG_URL_PATTERN_LABEL = "URL-Pattern:*";
     
   }
   
   public static class AddJSFProjectTemplateDialog{
     public static final String NAME_TEXT_LABEL = "Name:*";
     public static final String IMPLEMENTATION_COMBO_LABEL = "Implementation:*";
   }
   
   public static class NewViewDialog{
     public static final String FROM_VIEW_ID_TEXT_LABEL = "From View ID:";
     public static final String TEMPLATE_TEXT_LABEL = "Template:*";
     public static final String CREATE_FILE_ON_DISK_CHECKBOX_LABEL = "Create File on Disk";
   }
   
   public static class PropertiesEditor{
     
     public static final String SOURCE_TAB_LABEL = "Source";
     public static final String PROPERTIES_TAB_LABEL = "Properties";
     public static final String ADD_PROPERTIES_DIALOG_NAME_LABEL = "Name:*";
     public static final String ADD_PROPERTIES_DIALOG_VALUE_LABEL = "Value:";
     public static final String EDIT_PROPERTIES_DIALOG_NAME_LABEL = "Name:*";
     public static final String EDIT_PROPERTIES_DIALOG_VALUE_LABEL = "Value:";
     
   }
 }
