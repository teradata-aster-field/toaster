context("computeGraph")

test_that("toaGraph throws errors", {
  
  expect_error(toaGraph(NULL, NULL),
               "An edge table must be defined.")
  
  expect_error(toaGraph("vertices", NULL),
               "An edge table must be defined.")
  
  expect_error(toaGraph("vertices"),
               "An edge table must be defined.")
  
  expect_error(toaGraph(NULL, "edges", vertexWhere = "some condition"),
               "Graph with derived vertices can not have where clause.")
})

test_that("computeGraph throws errors", {
  
  expect_error(computeGraph(NULL, "not graph"),
               "Graph object must be specified.")
  
  expect_error(computeGraph(NULL, toaGraph("vs", "es"), test=TRUE),
               "Must provide allTables when test==TRUE.")
  
  expect_error(computeGraph(NULL, toaGraph("vs", "es"), v=list(logical(1)), 
                            allTables = data.frame(TABLE_NAME=c("vs","_es_"), stringsAsFactors = FALSE), test=TRUE),
               "Both vertices and edges must exist as tables or views.")
  
  expect_error(computeGraph(NULL, toaGraph(edges = "ed"),
                            vertexWhere = "weight > 0",
                            allTables = data.frame(TABLE_NAME=c("ed"), stringsAsFactors = FALSE), test=TRUE),
               "Derived vertices can not have where clause.")
  
  expect_error(computeGraph(NULL, toaGraph("vs", "es"), v=list(logical(1)), 
                            allTables = data.frame(TABLE_NAME=c("vs","es"), stringsAsFactors = FALSE), test=TRUE),
               ".*Values must be either numeric or character only.")
})


simplestGraph = toaGraph("vertices", "edges")
simplestGraphWithEdgeAttrs = toaGraph("vertices", "edges", edgeAttrnames = c("weight","cost"))
vertexWhereGraph = toaGraph("vertices", "edges", vertexWhere = "state='TX'")

test_that("computeGraph works properly", {
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, 
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT source, target FROM edges ;
                           --
                           -- Vertices Select
                           SELECT id 
                             FROM vertices  ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, v=character(0),
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT source, target FROM edges ;
                           --
                           -- Vertices Select
                           SELECT id 
                             FROM vertices  ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, v=list(),
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT source, target FROM edges ;
                           --
                           -- Vertices Select
                           SELECT id 
                             FROM vertices  ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraphWithEdgeAttrs,
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT source, target, weight, cost FROM edges ;
                           --
                           -- Vertices Select
                           SELECT id 
                             FROM vertices  ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, vertexWhereGraph,
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                            SELECT source, target FROM edges 
                            WHERE source IN (SELECT id FROM vertices WHERE state='TX' )
                              AND target IN (SELECT id FROM vertices WHERE state='TX' ) ;
                           -- -- Vertices Select 
                           SELECT id FROM vertices WHERE state='TX' ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, v=list(1,2,3),
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                            SELECT source, target FROM edges 
                            WHERE source IN (SELECT id FROM vertices WHERE id IN (1, 2, 3) )
                              AND target IN (SELECT id FROM vertices WHERE id IN (1, 2, 3) ) ;
                           -- -- Vertices Select 
                           SELECT id FROM vertices WHERE id IN (1, 2, 3) ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, v=list('1','2','3'),
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                            SELECT source, target FROM edges 
                            WHERE source IN (SELECT id FROM vertices WHERE id IN ('1', '2', '3') )
                              AND target IN (SELECT id FROM vertices WHERE id IN ('1', '2', '3') ) ;
                           -- -- Vertices Select 
                           SELECT id FROM vertices WHERE id IN ('1', '2', '3') ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, simplestGraph, v=list('a','b','c'),
                                       allTables = data.frame(TABLE_NAME=c("vertices","edges"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                            SELECT source, target FROM edges 
                            WHERE source IN (SELECT id FROM vertices WHERE id IN ('a', 'b', 'c') )
                              AND target IN (SELECT id FROM vertices WHERE id IN ('a', 'b', 'c') ) ;
                          -- -- Vertices Select 
                           SELECT id FROM vertices WHERE id IN ('a', 'b', 'c') ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, 
             toaGraph("graph.films_vertices", "graph.films_edges", FALSE, "name", "name1", "name2",
                      vertexAttrnames = "role", 
                      edgeAttrnames = c("weight","years")),
                      allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                             TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
             "-- Edges Select
             SELECT name1, name2, weight, years 
               FROM graph.films_edges ;
             --
             -- Vertices Select
             SELECT name, role FROM graph.films_vertices ORDER BY name")
  
  expect_equal_normalized(computeGraph(NULL,
                                       toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                "name", "name1", "name2", 
                                                vertexAttrnames = "role",
                                                vertexWhere = "role = 'Actor'"),
                                                allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                             TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
                      
                      "-- Edges Select
                      SELECT name1, name2 
                        FROM graph.films_edges
                       WHERE name1 IN (SELECT name FROM graph.films_vertices WHERE role = 'Actor' )
                         AND name2 IN (SELECT name FROM graph.films_vertices WHERE role = 'Actor' ) ;
                      --
                      -- Vertices Select
                      SELECT name, role FROM graph.films_vertices
                       WHERE role = 'Actor' 
                       ORDER BY name")
  
  expect_equal_normalized(computeGraph(NULL,
                                       toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                "name", "name1", "name2",
                                                vertexAttrnames = "role",
                                                vertexWhere = "role = 'Actor'"),
                                       v = list("Cruz", "Pacino", "Beluci", "Portman"),
                                       allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                                              TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
                      
                      "-- Edges Select
                      SELECT name1, name2 
                        FROM graph.films_edges
                       WHERE name1 IN (SELECT name FROM graph.films_vertices WHERE (role = 'Actor') AND 
                                                                                    name IN ('Cruz', 'Pacino', 'Beluci', 'Portman') )
                         AND name2 IN (SELECT name FROM graph.films_vertices WHERE (role = 'Actor') AND
                                                                                    name IN ('Cruz', 'Pacino', 'Beluci', 'Portman') ) ;
                      --
                      -- Vertices Select
                      SELECT name, role FROM graph.films_vertices
                       WHERE (role = 'Actor') AND name IN ('Cruz', 'Pacino', 'Beluci', 'Portman')
                       ORDER BY name",
                      label="With vertex attr, vertex where, v list")
  
  expect_equal_normalized(computeGraph(NULL,
                                       toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                "name", "name1", "name2",
                                                vertexAttrnames = "role",
                                                vertexWhere = "role = 'Actor'"),
                                       edgeWhere = "years > 0",
                                       v = list("Cruz", "Pacino", "Beluci", "Portman"),
                                       allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                                              TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
                      
                      "-- Edges Select
                      SELECT name1, name2 
                        FROM graph.films_edges
                       WHERE (years > 0) 
                         AND name1 IN (SELECT name FROM graph.films_vertices WHERE (role = 'Actor') AND 
                                                                                    name IN ('Cruz', 'Pacino', 'Beluci', 'Portman') )
                         AND name2 IN (SELECT name FROM graph.films_vertices WHERE (role = 'Actor') AND
                                                                                    name IN ('Cruz', 'Pacino', 'Beluci', 'Portman') ) ;
                      --
                      -- Vertices Select
                      SELECT name, role FROM graph.films_vertices
                       WHERE (role = 'Actor') AND name IN ('Cruz', 'Pacino', 'Beluci', 'Portman')
                       ORDER BY name",
                      label="With vertex attr, vertex where, edge where, v list")
  
  expect_equal_normalized(computeGraph(NULL,
                                       toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                "name", "name1", "name2",
                                                vertexAttrnames = "role"),
                                       v = "SELECT name FROM graph.films_vertices WHERE name like '%Bill%' or name like '%Tom%'",
                                       allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                                              TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
                      
                      "-- Edges Select
                      SELECT name1, name2 
                        FROM graph.films_edges
                       WHERE name1 IN (SELECT name FROM graph.films_vertices
                                        WHERE name IN (SELECT name FROM graph.films_vertices WHERE name like '%Bill%' or name like '%Tom%') )
                         AND name2 IN (SELECT name FROM graph.films_vertices 
                                        WHERE name IN (SELECT name FROM graph.films_vertices WHERE name like '%Bill%' or name like '%Tom%') ) ;
                      --
                      -- Vertices Select
                      SELECT name, role FROM graph.films_vertices
                       WHERE name IN (SELECT name FROM graph.films_vertices WHERE name like '%Bill%' or name like '%Tom%')
                       ORDER BY name",
                      label="With vertex attr, v SELECT")
  
  expect_equal_normalized(computeGraph(NULL,
                                       toaGraph("graph.films_vertices", "graph.films_edges", FALSE,
                                                "name", "name1", "name2",
                                                vertexAttrnames = "role",
                                                vertexWhere = "role = 'Actor'"),
                                       v = "SELECT name FROM graph.films_vertices WHERE name like '%Bill%' or name like '%Tom%'",
                                       allTables = data.frame(TABLE_SCHEM=c("graph","graph"),
                                                              TABLE_NAME=c("films_vertices","films_edges")),
                      test=TRUE),
                      
                      "-- Edges Select
                      SELECT name1, name2 
                        FROM graph.films_edges
                       WHERE name1 IN (SELECT name FROM graph.films_vertices 
                                        WHERE (role = 'Actor') 
                                          AND name IN (SELECT name FROM graph.films_vertices WHERE name like '%Bill%' or name like '%Tom%') )
                         AND name2 IN (SELECT name FROM graph.films_vertices 
                                        WHERE (role = 'Actor') 
                                          AND name IN (SELECT name FROM graph.films_vertices WHERE name like '%Bill%' or name like '%Tom%') ) ;
                      --
                      -- Vertices Select
                      SELECT name, role FROM graph.films_vertices
                       WHERE (role = 'Actor') 
                         AND name IN (SELECT name FROM graph.films_vertices WHERE name like '%Bill%' or name like '%Tom%')
                       ORDER BY name",
                      label="With vertex attr, vertex where, v SELECT")
  
})


simplestNovertexGraph = toaGraph(edges = "dallaspolice_officer_edges_un", 
                                 source = "officer1", target = "officer2")
simplestNovertexGraphWithEdgeAttrs = toaGraph(edges = "dallaspolice_officer_edges_un", 
                                              source = "officer1", target = "officer2",
                                              edgeAttrnames = c("weight","cost"))
novertexWhereGraph = toaGraph(edges = "dallaspolice_officer_edges_un", 
                              source = "officer1", target = "officer2",
                              edgeWhere = "weight >=  0.1")

test_that("computeGraph with derived vertex table works properly", {
  
  expect_equal_normalized(computeGraph(NULL, simplestNovertexGraph,
                                       allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_edges_un"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT officer1, officer2 FROM dallaspolice_officer_edges_un ;
                           --
                           -- Vertices Select
                           SELECT id 
                             FROM (SELECT officer1 id 
                                     FROM dallaspolice_officer_edges_un 
                                   UNION
                                   SELECT officer2 id 
                                     FROM dallaspolice_officer_edges_un ) t  ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, simplestNovertexGraph, v=character(0),
                                       allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_edges_un"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT officer1, officer2 FROM dallaspolice_officer_edges_un ;
                           --
                           -- Vertices Select
                           SELECT id 
                             FROM (SELECT officer1 id 
                                     FROM dallaspolice_officer_edges_un 
                                   UNION
                                   SELECT officer2 id 
                                     FROM dallaspolice_officer_edges_un ) t  ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, simplestNovertexGraph, v=list(),
                                       allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_edges_un"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT officer1, officer2 FROM dallaspolice_officer_edges_un ;
                           --
                           -- Vertices Select
                           SELECT id 
                             FROM (SELECT officer1 id 
                                     FROM dallaspolice_officer_edges_un 
                                   UNION
                                   SELECT officer2 id 
                                     FROM dallaspolice_officer_edges_un ) t  ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, simplestNovertexGraphWithEdgeAttrs,
                                       allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_edges_un"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                           SELECT officer1, officer2, weight, cost FROM dallaspolice_officer_edges_un ;
                           --
                           -- Vertices Select
                           SELECT id 
                             FROM (SELECT officer1 id 
                                     FROM dallaspolice_officer_edges_un 
                                   UNION
                                   SELECT officer2 id 
                                     FROM dallaspolice_officer_edges_un ) t  ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, novertexWhereGraph,
                                       allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_edges_un"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
                            SELECT officer1, officer2 FROM dallaspolice_officer_edges_un 
                             WHERE weight >=  0.1 ;
                           -- 
                           -- Vertices Select 
                           SELECT id 
                             FROM (SELECT officer1 id 
                                     FROM dallaspolice_officer_edges_un WHERE weight >=  0.1 
                                   UNION
                                   SELECT officer2 id 
                                     FROM dallaspolice_officer_edges_un WHERE weight >=  0.1
                              ) t  ORDER BY id")
  
  expect_equal_normalized(computeGraph(NULL, simplestNovertexGraph, v=list('10001', '10002', '10003', '10004', 
                                                                           '10005', '10133', '10331', '10030'),
                                       allTables = data.frame(TABLE_NAME=c("dallaspolice_officer_edges_un"), stringsAsFactors = FALSE),
                                       test=TRUE),
                          "-- Edges Select
      SELECT officer1, officer2 
         FROM dallaspolice_officer_edges_un WHERE officer1 IN (SELECT id 
       FROM (SELECT officer1 id 
       FROM dallaspolice_officer_edges_un 
     UNION
     SELECT officer2 id 
       FROM dallaspolice_officer_edges_un ) t WHERE id IN ('10001', '10002', '10003', '10004', '10005', '10133', '10331', '10030')  ) AND 
     officer2 IN (SELECT id 
       FROM (SELECT officer1 id 
       FROM dallaspolice_officer_edges_un 
     UNION
     SELECT officer2 id 
       FROM dallaspolice_officer_edges_un ) t WHERE id IN ('10001', '10002', '10003', '10004', '10005', '10133', '10331', '10030')  )  ;
    --
    -- Vertices Select
    SELECT id 
       FROM (SELECT officer1 id 
       FROM dallaspolice_officer_edges_un 
     UNION
     SELECT officer2 id 
       FROM dallaspolice_officer_edges_un ) t WHERE id IN ('10001', '10002', '10003', '10004', '10005', '10133', '10331', '10030')   ORDER BY id")

  
  expect_equal_normalized(computeGraph(NULL, toaGraph(edges = "graph.films_edges", directed = FALSE,
                                                      key = "name", source = "name1", target = "name2"),
                                       edgeWhere = "weight > 1",
                                       allTables = data.frame(TABLE_SCHEM=c("graph"),
                                                              TABLE_NAME=c("films_edges")),
                                       test=TRUE),
                      
                      "-- Edges Select
                      SELECT name1, name2 
                        FROM graph.films_edges WHERE weight > 1  ;
                      --
                      -- Vertices Select
                      SELECT name
                        FROM (SELECT name1 name 
                                FROM graph.films_edges WHERE weight > 1  
                              UNION
                              SELECT name2 name 
                                FROM graph.films_edges WHERE weight > 1  ) t  
                       ORDER BY name")
  
  expect_equal_normalized(computeGraph(NULL, toaGraph(edges = "graph.films_edges", directed = FALSE,
                                                      key = "name", source = "name1", target = "name2"),
                                       v = list('Andy Wachowski', 'Jim Broadbent', 'Halle Berry', 'Hugo Weaving'),
                                       allTables = data.frame(TABLE_SCHEM=c("graph"),
                                                              TABLE_NAME=c("films_edges")),
                                       test=TRUE),
                      
                      "-- Edges Select
                       SELECT name1, name2 
                         FROM graph.films_edges 
                        WHERE name1 IN (SELECT name 
                                          FROM (SELECT name1 name 
                                                  FROM graph.films_edges 
                                                UNION
                                                SELECT name2 name 
                                                  FROM graph.films_edges ) t 
                                         WHERE name IN 
                            ('Andy Wachowski', 'Jim Broadbent', 'Halle Berry', 'Hugo Weaving')  )
                          AND name2 IN (SELECT name 
                                          FROM (SELECT name1 name 
                                                  FROM graph.films_edges 
                                                 UNION
                                                SELECT name2 name 
                                                  FROM graph.films_edges ) t 
                                         WHERE name IN 
                            ('Andy Wachowski', 'Jim Broadbent', 'Halle Berry', 'Hugo Weaving')  )  ;
                        --
                        -- Vertices Select
                        SELECT name 
                          FROM (SELECT name1 name 
                                  FROM graph.films_edges 
                                UNION
                                SELECT name2 name 
                                  FROM graph.films_edges ) t 
                         WHERE name IN 
                           ('Andy Wachowski', 'Jim Broadbent', 'Halle Berry', 'Hugo Weaving')   
                         ORDER BY name")
  
  expect_equal_normalized(computeGraph(NULL,
                                       toaGraph(edges = "graph.films_edges", directed = FALSE,
                                                key = "name", source = "name1", target = "name2"),
                                       edgeWhere = "years >= 0",
                                       v = list('Andy Wachowski', 'Jim Broadbent', 'Halle Berry', 'Hugo Weaving'),
                                       allTables = data.frame(TABLE_SCHEM=c("graph"),
                                                              TABLE_NAME=c("films_edges")),
                                       test=TRUE),
                      
                      "-- Edges Select
                       SELECT name1, name2 
                         FROM graph.films_edges 
                        WHERE (years >= 0) 
                          AND name1 IN (SELECT name 
                                          FROM (SELECT name1 name 
                                                  FROM graph.films_edges WHERE years >= 0
                                                UNION
                                                SELECT name2 name 
                                                  FROM graph.films_edges WHERE years >= 0 ) t 
                                         WHERE name IN 
                            ('Andy Wachowski', 'Jim Broadbent', 'Halle Berry', 'Hugo Weaving')  )
                          AND name2 IN (SELECT name 
                                          FROM (SELECT name1 name 
                                                  FROM graph.films_edges WHERE years >= 0
                                                 UNION
                                                SELECT name2 name 
                                                  FROM graph.films_edges WHERE years >= 0 ) t 
                                         WHERE name IN 
                            ('Andy Wachowski', 'Jim Broadbent', 'Halle Berry', 'Hugo Weaving')  )  ;
                        --
                        -- Vertices Select
                        SELECT name 
                          FROM (SELECT name1 name 
                                  FROM graph.films_edges WHERE years >= 0
                                UNION
                                SELECT name2 name 
                                  FROM graph.films_edges WHERE years >= 0 ) t 
                         WHERE name IN 
                           ('Andy Wachowski', 'Jim Broadbent', 'Halle Berry', 'Hugo Weaving')   
                         ORDER BY name")
  
  expect_equal_normalized(computeGraph(NULL,
                                       toaGraph(edges = "graph.films_edges", directed = FALSE,
                                                key = "name", source = "name1", target = "name2",
                                                edgeAttrnames = c("weight","years")),
                                       v = "SELECT name FROM graph.films_vertices 
                                             WHERE name like '%Bill%' or name like '%Tom%'",
                                       allTables = data.frame(TABLE_SCHEM=c("graph"),
                                                              TABLE_NAME=c("films_edges")),
                                       test=TRUE),
                      
                      "-- Edges Select
                      SELECT name1, name2, weight, years 
                        FROM graph.films_edges 
                       WHERE name1 IN (SELECT name 
                                         FROM (SELECT name1 name 
                                                 FROM graph.films_edges 
                                               UNION
                                               SELECT name2 name 
                                                 FROM graph.films_edges ) t 
                                        WHERE name IN (SELECT name FROM graph.films_vertices 
                                                        WHERE name like '%Bill%' or name like '%Tom%')  ) 
                         AND name2 IN (SELECT name 
                                         FROM (SELECT name1 name 
                                                 FROM graph.films_edges 
                                                UNION
                                               SELECT name2 name 
                                                 FROM graph.films_edges ) t 
                                        WHERE name IN (SELECT name FROM graph.films_vertices 
                                                        WHERE name like '%Bill%' or name like '%Tom%')  )  ;
                       --
                       -- Vertices Select
                       SELECT name 
                         FROM (SELECT name1 name 
                                 FROM graph.films_edges 
                               UNION
                               SELECT name2 name 
                                 FROM graph.films_edges ) t 
                        WHERE name IN (SELECT name FROM graph.films_vertices 
                                        WHERE name like '%Bill%' or name like '%Tom%')   
                        ORDER BY name",
                      label="With vertex attr, v SELECT")
  
})